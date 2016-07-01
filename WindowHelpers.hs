module WindowHelpers where

import Control.DeepSeq (NFData, force)
import Control.Monad (when)
import Data.Maybe (isJust, fromJust)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Graphics.HGL as HGL

import FRP.Yampa
import FRP.Yampa.Task (repeatUntil, forAll)

type WinInput = Event HGL.Event
type Frequency = Double

animate ::
  Frequency ->
    String ->
    (Int, Int) ->
    SF WinInput HGL.Graphic ->
    IO ()
animate fps title size signalFn = HGL.runGraphics $ do
  win <- HGL.openWindowEx title
    Nothing
    size
    HGL.DoubleBuffered
    (Just 1)
  (init, getTimeInput, isClosed) <- mkInitAndGetTimeInput win

  reactimate
    init
    getTimeInput
    (\a b -> doRender win a b >> isClosed) $
    (repeatedly (1 / fps) () &&& signalFn) &&&
      (repeatedly 1 () &&& loop
        (arr ((+ 1) . snd) >>> iPre (0 :: Int) >>> arr dup))

  HGL.closeWindow win
  where
    doRender win _ (ea@(e, a), (e', c)) =
      updateWin win ea
      --forAll (showState a) putStrLn

------------------------------------------------------------------------------
-- Support for reading time and input
------------------------------------------------------------------------------

mkInitAndGetTimeInput ::
  HGL.Window ->
    IO (
        IO WinInput,
        Bool -> IO (DTime, Maybe WinInput),
        IO Bool
      )
mkInitAndGetTimeInput win = do
  -- clkRes   <- fmap fromIntegral (getSysVar ClockTick)
  let clkRes = 1000
  tpRef     <- newIORef errInitNotCalled
  wepRef    <- newIORef errInitNotCalled
  weBufRef  <- newIORef Nothing
  closedRef <- newIORef False
  let init = do
      t0 <- getElapsedTime
      writeIORef tpRef t0
      mwe <- getWinInput win weBufRef
      writeIORef wepRef mwe
      return (maybeToEvent mwe)
  let getTimeInput _ = do
      tp <- readIORef tpRef
      t  <- getElapsedTime `repeatUntil` (/= tp) -- Wrap around possible!
      let dt = if t > tp then fromIntegral (t-tp)/clkRes else 1/clkRes
      writeIORef tpRef t
      mwe  <- getWinInput win weBufRef
      mwep <- readIORef wepRef
      writeIORef wepRef mwe
      -- Simplistic "delta encoding": detects only repeated NoEvent.
      case (mwep, mwe) of
        (Nothing, Nothing)   ->
          return (dt, Nothing)
        (_, Just HGL.Closed) -> do
          writeIORef closedRef True
          return (dt, Just(maybeToEvent mwe))
        (_, mev)             ->
          return (dt, Just (maybeToEvent mev))
  return (init, getTimeInput, readIORef closedRef)
  where
    errInitNotCalled = intErr
      "RSAnimate"
      "mkInitAndGetTimeInput"
      "Init procedure not called."

    -- Accurate enough? Resolution seems to be 0.01 s, which could lead
    -- to substantial busy waiting above.
    -- getElapsedTime :: IO ClockTick
    -- getElapsedTime = fmap elapsedTime getProcessTimes

    -- Use this for now. Have seen delta times down to 0.001 s. But as
    -- the complexity of the simulator signal function gets larger, the
    -- processing time for one iteration will presumably be > 0.01 s,
    -- and a clock resoltion of 0.01 s vs. 0.001 s becomes a non issue.
    getElapsedTime :: IO HGL.Time
    getElapsedTime = HGL.getTime

    maybeToEvent :: Maybe a -> Event a
    maybeToEvent = maybe NoEvent Event


-- Get window input, with "redundant" mouse moves removed.
getWinInput :: HGL.Window -> IORef (Maybe HGL.Event) -> IO (Maybe HGL.Event)
getWinInput win weBufRef = do
  mwe <- readIORef weBufRef
  case mwe of
    Just _  -> do
      writeIORef weBufRef Nothing
      return mwe
    Nothing -> do
      mwe' <- gwi win
      case mwe' of
        Just (HGL.MouseMove {}) -> mmFilter mwe'
        _                       -> return mwe'
  where
    mmFilter jmme = do
      mwe' <- gwi win
      case mwe' of
        Nothing                 -> return jmme
        Just (HGL.MouseMove {}) -> mmFilter mwe'
        Just _                  -> writeIORef weBufRef mwe' >> return jmme

      -- Seems as if we either have to yield or wait for a tick in order
      -- to ensure that the thread receiving events gets a chance to
      -- work. For some reason, yielding seems to result in window close
      -- events getting through, wheras waiting often means they don't.
      -- Maybe the process typically dies before the waiting time is up in
      -- the latter case?
    gwi win = do
      HGL.getWindowTick win
      mwe <- HGL.maybeGetWindowEvent win
      return mwe


------------------------------------------------------------------------------
-- Support for output
------------------------------------------------------------------------------

-- Need to force non-displayed elements to avoid space leaks.
-- We also explicitly force displayed elements in case the renderer does not
-- force everything.
updateWin :: HGL.Window -> (Event (), HGL.Graphic) -> IO ()
updateWin win (e, a) = when (isEvent e) $
  HGL.setGraphic win a
intErr mod fn msg = error $ concat [mod, ": [", fn, "] ", msg]
