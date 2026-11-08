{-# LANGUAGE TypeFamilies #-}

module Oppentimer.Text (centerText) where

import Apecs.Gloss (Picture, text, translate)
import Graphics.UI.GLUT (GLint)
import qualified Graphics.UI.GLUT.Fonts as Fonts

textWidth :: String -> IO GLint
textWidth = Fonts.stringWidth Fonts.Roman

centerText :: String -> IO Picture
centerText text' = do
        fontWidth <- textWidth text'
        let renderedText = text text'
        pure $ translate (-fromIntegral fontWidth / 2) 0 $ renderedText
