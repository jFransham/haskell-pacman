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
      (AnyOf [KeyChar 'w', KeyUp])
      (AnyOf [KeyChar 's', KeyDown])
      (AnyOf [KeyChar 'a', KeyLeft])
      (AnyOf [KeyChar 'd', KeyRight])
    tAtan2 = uncurry . flip $ atan2 -- atans (x, y) tuple

playerVisuals :: SF (Radians, Point2 Double) HGL.Graphic
playerVisuals =
  let (minOpen, maxOpen) = (0.1, 0.6)
      semiOpen           = minOpen + (maxOpen - minOpen) / 2
      openingAmounts     = [minOpen, semiOpen, maxOpen] in
    proc (theta, pos) -> do
      ev   <- repeatedly 0.05 () -< ()
      open <- accumHold 0 -<
        ev `tag` ((`mod` pingPongListLength openingAmounts) . (+ 1))
      let openAmount   = openingAmounts `pingPongListElement` open
          (upT, downT) = (theta - openAmount, theta + openAmount)
      returnA -<
        HGL.withRGB (HGL.RGB 255 255 0) $
          exceptArc
            upT
            downT
            10
            (rotatePoint theta (vector2 0 0.3))
            pos

pingPongListElement list i =
  let index  = i `mod` pingPongListLength list
      end    = length list - 1
      lIndex = if index > end then end - (index `mod` end) else index
  in list !! lIndex
pingPongListLength list = length list + (max (length list - 2) 0)

-- 0 is at north, rotates clockwise
exceptArc ::
  Radians -> Radians -> Double -> Vector2 Double -> Point2 Double -> HGL.Graphic
exceptArc start end radius offset (Point2 x y) = HGL.regionToGraphic $
  HGL.ellipseRegion
    (both truncate $ (x - radius, y - radius))
    (both truncate $ (x + radius, y + radius)) `HGL.subtractRegion`
      HGL.polygonRegion triangle
  where
    r' = radius / 2
    triangle = map (
        both truncate .
        (\(x', y') -> (x + x', y + y')) .
        both (* radius) .
        vector2XY .
        (^+^ offset)
      ) $ arcTriangleApprox start end

arcTriangleApprox start end =
  [vector2 0 0,
    rotatePoint start (vector2 0 $ -2),
    rotatePoint end (vector2 0 $ -2)]

rotatePointAround :: Radians -> Point2 Double -> Point2 Double -> Point2 Double
rotatePointAround th origin pnt = pnt .+^ rotatePoint th (pnt .-. origin)

rotatePoint :: Radians -> Vector2 Double -> Vector2 Double
rotatePoint th = uncurry vector2 . rotateTup th . vector2XY
  where
    rotateTup th (x, y) = (c * x - s * y, s * x + c * y)
    c = cos th
    s = sin th

moveVecNorm ::
  Keycode ->
    Keycode ->
    Keycode ->
    Keycode ->
    SF WinInput (Event (Vector2 Double))
moveVecNorm up down left right = proc input -> do
  up    <- keyDown up    -< input
  down  <- keyDown down  -< input
  left  <- keyDown left  -< input
  right <- keyDown right -< input
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
