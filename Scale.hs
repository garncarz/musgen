module Scale where

import List
import Maybe

import Pitch

type Intervals = [Int]
data Scale = Scale Pitch Intervals deriving (Eq, Show)

scalePitch :: Scale -> Int -> Pitch
scalePitch (Scale p i) pos =
	addPitch p (pos `div` (length i) * octaveInterval +
		i !! (pos `mod` (length i)))

octaveInterval :: Int
octaveInterval = 12

major :: Intervals
major = [0, 2, 4, 5, 7, 9, 11]

minor :: Intervals
minor = [0, 2, 3, 5, 7, 8, 10]


cMaj = Scale (Pitch "c" 1) major
aMin = Scale (Pitch "a" 1) minor

