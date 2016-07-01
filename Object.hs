{-# LANGUAGE Arrows, DuplicateRecordFields #-}

module Object where

import WindowHelpers
import IdentityList
import Keycodes

import qualified Graphics.HGL as HGL
import System.Random
import Control.DeepSeq (NFData, force)
import Control.Monad (when, join)
import Data.Maybe (isJust, fromJust)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (sortBy)
import Data.Char
import Data.Tuple.Extra (both)

import FRP.Yampa
import FRP.Yampa.Point2
import FRP.Yampa.Vector2
import FRP.Yampa.VectorSpace
import FRP.Yampa.AffineSpace
import FRP.Yampa.Task (repeatUntil, forAll)

type AABB a = (a, a, a, a)

data ObjOutput a = ObjOutput {
    bbox :: AABB a,
    position :: Point2 a,
    visual :: HGL.Graphic
  }

data ObjInput a = ObjInput {
    impulse :: Vector2 a,
    input :: WinInput
  }

type Object a = SF (ObjInput a) (ObjOutput a)

type Radians = Double

player = playerMovement >>> playerVisuals

playerMovement :: SF WinInput (Radians, Point2 Double)
playerMovement = let moveSpeed = 100 in
  proc input -> do
    vel      <- vecFromInput            -< input
    angle    <- accumHold 0             -< fmap (const . tAtan2 . vector2XY) vel
    movement <- accumHold (vector2 0 0) -< fmap const vel
    pos <- (p0 .+^) ^<< integral -< moveSpeed *^ movement
    returnA -< (angle + pi / 2, pos)
  where
    p0 = Point2 100.0 100.0
    vecFromInput = moveVecNorm
      [KeyChar 'w', KeyUp]
      [KeyChar 's', KeyDown]
      [KeyChar 'a', KeyLeft]
      [KeyChar 'd', KeyRight]
    tAtan2 = uncurry . flip $ atan2

playerVisuals :: SF (Radians, Point2 Double) HGL.Graphic
playerVisuals =
  proc (theta, Point2 x y) -> do
    ev   <- repeatedly 0.2 () -< ()
    open <- accumHold False   -< ev `tag` not
    let openAmount   = if open then 0.5 else 0.05
        mouthHeading = theta
        (upT, downT) = (mouthHeading - openAmount, mouthHeading + openAmount)
    returnA -< exceptArc upT downT 10 (Point2 x y)

-- 0 is at north, rotates clockwise
exceptArc :: Radians -> Radians -> Double -> Point2 Double-> HGL.Graphic
exceptArc start end radius (Point2 x y) = HGL.regionToGraphic $
  HGL.ellipseRegion
    (both truncate (x - radius, y - radius))
    (both truncate (x + radius, y + radius)) `HGL.subtractRegion`
    HGL.polygonRegion triangle
  where
    r' = radius / 2
    triangle = map (
        both truncate .
        (\(x', y') -> (x + x', y + y')) .
        both (* radius)
      ) $ arcTriangleApprox start end

arcTriangleApprox start end =
  [(0, 0), rotatePoint start (0, -2), rotatePoint end (0, -2)]

rotatePoint :: Double -> (Double, Double) -> (Double, Double)
rotatePoint th (x, y) = (c * x - s * y, s * x + c * y)
  where
    c = cos th
    s = sin th

moveVecNorm ::
  [Keycode] ->
    [Keycode] ->
    [Keycode] ->
    [Keycode] ->
    SF WinInput (Event (Vector2 Double))
moveVecNorm up down left right = proc input -> do
  up    <- anyKey up    -< input
  down  <- anyKey down  -< input
  left  <- anyKey left  -< input
  right <- anyKey right -< input
  returnA -< fmap normalize .
    filterE (/= vector2 0 0) .
    fmap (uncurry vector2) .
    foldl1 lMerge $ [
        toEvent left  (-1, 0),
        toEvent right (1,  0),
        toEvent up    (0, -1),
        toEvent down  (0,  1)
      ]

limitPoint (Point2 minX minY) (Point2 maxX maxY) (Point2 x y) =
  Point2 (limit minX maxX x) (limit minY maxY y)

limit min max val =
  if val < min then min
  else if val > max then max
  else val

within ax ay bx by (Point2 x y) = x > ax && x < bx && y > ay && y < by

safeNormalize vec = if vec == vector2 0 0
  then vector2 0 0
  else normalize vec

toEvent :: Bool -> a -> Event a
toEvent b = if b then Event else const noEvent

discardE :: Event a -> Event ()
discardE = fmap $ const ()
