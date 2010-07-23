module ChanceRhythm (rhythmChance) where

import List
import Types

rhythmChance :: ChanceType
rhythmChance now past = foldl (*) 1
	(map (\(chance, factor) -> (** factor) $ chance now past) chances)

chances :: [(ChanceType, Float)]
chances = [
	(chanceBeatTime, 1),
	(chanceRightSpeed, 0.5),
	(chanceSomeRhythm, 0.5),
	(chanceCopyRhythm, 0.1)
	]

chanceBeatTime now _ = if (remain1 - dur1 >= 0) then 1 else floatZero
	where dur1 = dur now; remain1 = remain now

chanceRightSpeed now _
	| dur1 == 8 = 1
	| dur1 == 4 = 0.8
	| dur1 == 16 = 0.6
	| dur1 == 2 = 0.5
	| otherwise = floatMin
	where dur1 = dur now

chanceSomeRhythm now (past:_)
	| beat2 >= 4 * remain2 = 1
	| dur1 == dur2 = 0.9
	| dur1 == 2 * dur2 = 1
	| dur2 == 2 * dur1 = 1
	| otherwise = floatMin
	where dur1 = dur past; dur2 = dur now; beat2 = beat now;
		remain2 = remain now
chanceSomeRhythm _ [] = 1

chanceCopyRhythm now flow =
	if past == [] || isPrefixOf actual past then 1 else floatMin
	where
		actual = rhythmHead (now:flow) beat1 (beat1 - remain1 + dur1) 0
		past = rhythmHead (now:flow) beat1 (beat1 - remain1 + dur1) 1
		dur1 = dur now; beat1 = beat now; remain1 = remain now
		rhythmHead :: Flow -> Duration -> Duration -> Int -> [Duration]
		rhythmHead flow beat passed skip
			| passed < 0 || flow == [] = []
			| passed == 0 && skip <= 0 = []
			| passed == 0 = rhythmHead flow beat beat (skip - 1)
			| passed > 0 && skip > 0 = rhythmHead rest beat (passed - dur1) skip
			| otherwise = (rhythmHead rest beat (passed - dur1) skip) ++ [dur1]
			where
				(now:rest) = flow
				dur1 = dur now

