{-# LANGUAGE TypeFamilies #-}

module Oppentimer.Clock (Clock) where

import Apecs

newtype TimeSeconds = TimeSeconds Float

data Clock = Running TimeSeconds | Ended
instance Component Clock where type Storage Clock = Map Clock

tickClock :: TimeSeconds -> Clock -> Clock
tickClock dt (clock) = _

viewClock :: Clock -> Picture
viewClock clock = _
