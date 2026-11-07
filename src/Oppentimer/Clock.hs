{-# LANGUAGE TypeFamilies #-}

module Oppentimer.Clock (Clock, tickClock, viewClock, newClock) where

import Apecs
import Apecs.Gloss

newtype TimeSeconds = TimeSeconds Float

data Clock = Running TimeSeconds | Ended
instance Component Clock where type Storage Clock = Map Clock

newClock :: Float -> Clock
newClock = Running . TimeSeconds

tickClock :: Float -> Clock -> Clock
tickClock dt (Running (TimeSeconds n)) = Running $ TimeSeconds (n - dt)
tickClock _ Ended = Ended

viewClock :: Clock -> Picture
viewClock (Running (TimeSeconds n)) = Text ((show minutes) ++ ":" ++ (show seconds))
    where
        minutes = (floor n :: Int) `div` 60
        seconds = if (n - ((fromIntegral minutes) * 60) < 10) then "0" ++ (show seconds) else show ((floor n) - (minutes * 60))
viewClock (Ended) = Text ("0:00")
