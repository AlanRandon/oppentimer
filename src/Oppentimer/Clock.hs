module Oppentimer.Clock (Clock) where

import Apecs

newtype TimeSeconds = TimeSeconds Float

data Clock = Running TimeSeconds | Ended
instance Component Clock where type Storage Clock = Map Clock

tickClock :: TimeSeconds -> Clock -> Clock
tickClock dt Running (n) = Running (n - dt)
tickClock dt (Ended _) = (Ended 0)

viewClock :: Clock -> Picture
viewClock (Running n) = Text ( (show minutes) ++ ":" ++ (show seconds))
    where 
        minutes = floor (n `div` 60)
        seconds = if show (n - (minutes * 60) < 10) then "0" ++ (show seconds) else show (seconds n - (minutes * 60))
viewClock (Ended _) = Text ( "0:00")


