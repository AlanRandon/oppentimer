{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Oppentimer.Timer (Timer, tickTimer, newTimer, isExpired, getTime) where

import Apecs (Component (..), Map)
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

getTime :: Timer -> Float
getTime Ended = 0
getTime (Running (TimeSeconds t)) = t

tickTimer :: Float -> Timer -> Timer
tickTimer _ Ended = Ended
tickTimer dt (Running (TimeSeconds n)) =
        if newTime > 0
                then Running $ TimeSeconds newTime
                else Ended
    where
        newTime = (n - dt)

isExpired :: Timer -> Bool
isExpired Ended = True
isExpired _ = False
