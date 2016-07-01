{-# LANGUAGE Arrows #-}

module Game where

import WindowHelpers
import IdentityList
import Keycodes
import Object
import Graphics

import qualified Graphics.HGL as HGL
import System.Random
import Control.DeepSeq (NFData, force)
import Control.Monad (when, join)
import Data.Maybe (isJust, fromJust)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (sortBy)
import Data.Char

import FRP.Yampa
import FRP.Yampa.Point2
import FRP.Yampa.Vector2
import FRP.Yampa.VectorSpace
import FRP.Yampa.AffineSpace
import FRP.Yampa.Task (repeatUntil, forAll)

-- Single tick - either renders or moves to a new state
newtype GameState = GameState (SF WinInput (HGL.Graphic, Event GameState))

white :: HGL.RGB
white = HGL.RGB 255 255 255

runState :: GameState -> SF WinInput HGL.Graphic
runState (GameState initial) = dSwitch initial runState

initialState :: HGL.Brush -> GameState
initialState brush = GameState $ player >>^ (\x -> (x, noEvent))

{-
initialState :: RandomGen g => g -> GameState
initialState gen = GameState $
  parB
    [
      signalFn gen >>^
        (HGL.withTextColor white .
          HGL.withTextAlignment (HGL.Center, HGL.Top) .
          HGL.text (100, 100) .
          show),
      player >>^
        (\(x, y) ->
          HGL.polygon [
              (x - 10, y - 10),
              (x - 10, y + 10),
              (x + 10, y + 10),
              (x + 10, y - 10)
            ])
    ] >>^ ((\g -> (g, noEvent)) .
      foldr HGL.overGraphic (return ()))
      -- return () represents "draw nothing", as Graphic is a synonym for the
      -- Draw () monad
-}

signalFn :: RandomGen g => g -> SF WinInput Int
signalFn rand = let (init, gen) = next rand in
  proc input -> do
    tick   <- repeatedly 1 ()             -< ()
    down   <- edge <<< anyChar [' ', 'a'] -< input
    (i, _) <- accumHold (init, gen) -<
      (tick `lMerge` down) `tag` (next . snd)
    returnA -< i
