import WindowHelpers
import Game

import qualified Graphics.HGL as HGL
import System.Random
import Control.DeepSeq (NFData, force)
import Control.Monad (when)
import Data.Maybe (isJust, fromJust)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (sortBy)

import FRP.Yampa
import FRP.Yampa.Task (repeatUntil, forAll)

main :: IO ()
main = do
  gen <- getStdGen
  animate
    60
    "Hello, world!"
    (200, 200) $
    runState (initialState)
