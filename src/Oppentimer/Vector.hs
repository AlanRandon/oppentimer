module Oppentimer.Vector (addVec, hypotVec, multVec, negateVec, normaliseVec) where

addVec :: (Floating a) => (a, a) -> (a, a) -> (a, a)
addVec (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

hypotVec :: (Floating a) => (a, a) -> a
hypotVec (x, y) = sqrt ((x ** 2) + (y ** 2))

multVec :: (Floating a) => a -> (a, a) -> (a, a)
multVec s (x, y) = (x * s, y * s)

negateVec :: (Floating a) => (a, a) -> (a, a)
negateVec = multVec (-1)

normaliseVec :: (Floating a) => (a, a) -> (a, a)
normaliseVec v = (1 / hypotVec v) `multVec` v
