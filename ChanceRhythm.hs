module ChanceRhythm (rhythmChance) where

import List
import Types

rhythmChance :: ChanceType
rhythmChance now past = foldl (*) 1
	(map (\(chance, factor) -> (** factor) $ chance now past) chances)

chances :: [(ChanceType, Float)]
chances = [
	(chanceMeasureTime, 1),
	(chanceBeatTime, 0.35),
	(chanceRightSpeed, 0.5),
	(chanceSomeRhythm, 0.75),
	(chanceCopyRhythm, 0.9)
	]

chanceMeasureTime now _ = if (remain1 - dur1 >= 0) then 1 else floatZero
	where dur1 = dur now; remain1 = remain now

chanceBeatTime now _ =
	if (dur1 > beat || (remain1 - dur1) `mod` beat > 0) then floatHalf else 1
	where dur1 = dur now; remain1 = remain now;
		beat = measure now `div` beats now

chanceRightSpeed now _
	-- | even $ dur1 `div` 2 = floatHalf
	| dur1 < 8 = 1
	| dur1 == 8 = 0.9
	| dur1 > 8 = 0.6
	| otherwise = floatHalf
	where dur1 = dur now

chanceSomeRhythm now (past:_)
	| measure2 >= 4 * remain2 = 1
	| dur1 == dur2 = 0.75
	| dur1 == 2 * dur2 = 1
	| dur2 == 2 * dur1 = 1
	| otherwise = floatHalf
	where dur1 = dur past; dur2 = dur now; measure2 = measure now;
		remain2 = remain now
chanceSomeRhythm _ [] = 1

chanceCopyRhythm now flow =
	if past == [] || isPrefixOf actual past then 1 else floatHalf
	where
		actual = rhythmHead (now:flow) measure1 (measure1 - remain1 + dur1) 0
		past = rhythmHead (now:flow) measure1 (measure1 - remain1 + dur1) 1
		dur1 = dur now; measure1 = measure now; remain1 = remain now
		rhythmHead :: Flow -> Duration -> Duration -> Int -> [Duration]
		rhythmHead flow measure passed skip
			| passed < 0 || flow == [] = []
			| passed == 0 && skip <= 0 = []
			| passed == 0 = rhythmHead flow measure measure (skip - 1)
			| passed > 0 && skip > 0 =
				rhythmHead rest measure (passed - dur1) skip
			| otherwise =
				(rhythmHead rest measure (passed - dur1) skip) ++ [dur1]
			where
				(now:rest) = flow
				dur1 = dur now

