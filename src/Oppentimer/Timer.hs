{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Oppentimer.Timer (Timer, tickTimer, viewTimer, newTimer) where

import Apecs
import Apecs.Gloss
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

newTimer :: Float -> Timer
newTimer = Running . TimeSeconds

tickTimer :: Float -> Timer -> Timer
tickTimer dt (Running (TimeSeconds n)) = Running $ TimeSeconds (n - dt)
tickTimer _ Ended = Ended

viewTimer :: Timer -> Picture
viewTimer = translate 0 0 . scale 0.1 0.1 . color white . text . show
