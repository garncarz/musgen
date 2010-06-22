module Penalties where

import List
import Types

penalty :: Chord -> Float
penalty ch = penaltiesSum ch penalties / genericLength penalties

penaltiesSum :: Chord -> [Chord -> Float] -> Float
penaltiesSum _ [] = 0
penaltiesSum ch (p:ps) = p ch + penaltiesSum ch ps

penalties :: [Chord -> Float]
penalties = [penaltyDist, penaltySlow]

penaltyDist :: Chord -> Float
penaltyDist (tones, dur) = minimum [
	abs (realToFrac (sum tones) / genericLength tones - 60) / 10,
	1]

penaltySlow :: Chord -> Float
penaltySlow (_, dur)
	| dur > 5 = 1
	| otherwise = 0

