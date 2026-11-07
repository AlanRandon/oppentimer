{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Oppentimer.Timer (Timer (Ended), tickTimer, viewTimer, newTimer) where

import Apecs
import Apecs.Gloss
import Graphics.UI.GLUT (GLint)
import qualified Graphics.UI.GLUT.Fonts as Fonts
import Text.Printf (printf)

newtype TimeSeconds = TimeSeconds Float

data Timer = Running TimeSeconds | Ended
instance Component Timer where type Storage Timer = Map Timer

instance Show Timer where
        show (Running (TimeSeconds n)) = printf "%02d:%02d" minutes seconds
            where
                minutes = (floor n :: Int) `div` 60
                seconds = (floor n :: Int) `mod` 60
        show Ended = "00:00"

textWidth :: String -> IO GLint
textWidth = Fonts.stringWidth Fonts.Roman

-- centeredText str = do
--         GL.blend $= GL.Disabled
--         GL.preservingMatrix $ GLUT.renderString GLUT.Roman str
--         GL.blend $= GL.Enabled

newTimer :: Float -> Timer
newTimer = Running . TimeSeconds

tickTimer :: Float -> Timer -> Timer
tickTimer dt (Running (TimeSeconds n)) = Running $ TimeSeconds (n - dt)
tickTimer _ Ended = Ended

viewTimer :: Timer -> IO Picture
viewTimer timer = do
        let text' = show timer
        fontWidth <- textWidth text'
        let renderedText = text text'
        pure $ scale 0.1 0.1 $ translate (-fromIntegral fontWidth / 2) 0 $ renderedText
