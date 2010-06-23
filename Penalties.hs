module Penalties where

import List
import Types

penalty :: Chord -> Flow -> Float
penalty ch past = penaltiesSum ch past penalties

penaltiesSum :: Chord -> Flow -> [Chord -> Flow -> Float] -> Float
penaltiesSum _ _ [] = 0
penaltiesSum ch past (p:ps) = p ch past + penaltiesSum ch past ps

penalties :: [Chord -> Flow -> Float]
penalties = [penaltyJumps]

penaltyDist :: Chord -> Flow -> Float
penaltyDist (tones, dur) _ = minimum [
	abs (realToFrac (sum tones) / genericLength tones - 60) / 10,
	1]

penaltySlow :: Chord -> Flow -> Float
penaltySlow (_, dur) _
	| dur > 5 = 1
	| otherwise = 0


oneJump :: [Tone] -> Tone -> Int
oneJump pt t = fromEnum (not isNear) where
	tUp = t + 2
	tDown = t - 2
	isNear = elem tUp pt || elem tDown pt

penaltyJumps :: Chord -> Flow -> Float
penaltyJumps (tones, _) ((ptones, _):_)
	= fromIntegral (sum (map (oneJump ptones) tones)) / genericLength tones
penaltyJumps _ [] = 0

