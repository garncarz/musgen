module Penalties where

import List
import Types
import Relations

type PenalType = Chord -> Flow -> MusicState -> Float

penalty :: Chord -> Flow -> MusicState -> Float
penalty ch past st = penaltiesSum ch past st penalties

penaltiesSum :: Chord -> Flow -> MusicState -> [PenalType] -> Float
penaltiesSum _ _ _ [] = 0
penaltiesSum ch past st (p:ps) = p ch past st + penaltiesSum ch past st ps

penalties :: [PenalType]
penalties = [penaltyDist, penaltySlow, penaltyJumps,
	penaltyNotScale, penaltyNotTriad]

penaltyDist (tones, dur) _ (base, _) = minimum [
	abs (realToFrac (sum tones) / genericLength tones - fromIntegral base) / 10,
	1]

penaltySlow (_, dur) _ _
	| dur > 5 = 1
	| otherwise = 0

penaltyJumps (tones, _) ((ptones, _):_) _ =
	(sum $ map (\t -> if toneJump t ptones then 0 else 1) tones)
		/ genericLength tones
penaltyJumps _ [] _ = 0

penaltyNotScale (tones, _) _ (base, intervals) =
	sum $ map (\t -> if toneFromScale t base intervals then 0 else 1) tones

penaltyNotTriad (tones, _) _ (_, intervals) =
	if null tones || chordTriad tones intervals then 0 else 1


