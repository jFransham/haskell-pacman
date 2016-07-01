{-# LANGUAGE Arrows #-}

module Keycodes where

import WindowHelpers

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

data Keycode =
  AnyOf [Keycode] |
  KeyChar Char |
  KeyUp |
  KeyDown |
  KeyLeft |
  KeyRight |
  KeyEsc |
  KeyReturn |
  KeyTab |
  KeyPageUp |
  KeyPageDown |
  KeyShiftL |
  KeyShiftR |
  KeyCtrlL |
  KeyCtrlR |
  KeyEnd

anyKey :: [Keycode] -> SF WinInput Bool
anyKey keys = let keySignals = parB $ map keyDown keys in
  proc input -> do
    events <- keySignals -< input
    returnA -< foldr (||) False events

keyDown :: Keycode -> SF WinInput Bool
keyDown (KeyChar chr) = charDownI chr
keyDown (AnyOf codes) = anyKey codes
keyDown KeyUp       = keyPred HGL.isUpKey
keyDown KeyDown     = keyPred HGL.isDownKey
keyDown KeyLeft     = keyPred HGL.isLeftKey
keyDown KeyRight    = keyPred HGL.isRightKey
keyDown KeyEsc      = keyPred HGL.isEscapeKey
keyDown KeyReturn   = keyPred HGL.isReturnKey
keyDown KeyTab      = keyPred HGL.isTabKey
keyDown KeyPageUp   = keyPred HGL.isPageUpKey
keyDown KeyPageDown = keyPred HGL.isPageDownKey
keyDown KeyShiftL   = keyPred HGL.isShiftLKey
keyDown KeyShiftR   = keyPred HGL.isShiftRKey
keyDown KeyCtrlL    = keyPred HGL.isControlLKey
keyDown KeyCtrlR    = keyPred HGL.isControlRKey
keyDown KeyEnd      = keyPred HGL.isEndKey

keyPred :: (HGL.Key -> Bool) -> SF WinInput Bool
keyPred pred = proc input -> do
  let event = case input of
        Event (HGL.Key {HGL.keysym = k, HGL.isDown = down}) | pred k ->
            Event $ const down
        _ ->
          noEvent
  accumHold False -< event

keyPreds :: [HGL.Key -> Bool] -> SF WinInput Bool
keyPreds preds = let predSignals = parB $ map keyPred preds in
  proc input -> do
    events <- predSignals -< input
    returnA -< foldr (||) False events

charDownI :: Char -> SF WinInput Bool
charDownI c = anyChar [toLower c, toUpper c]

charDown :: Char -> SF WinInput Bool
charDown = keyPred . keyIsChar

keyIsChar :: Char -> HGL.Key -> Bool
keyIsChar c key = HGL.isCharKey key && c == HGL.keyToChar key

anyChar :: [Char] -> SF WinInput Bool
anyChar = keyPreds . map keyIsChar
