module ChanceRhythm (rhythmChance) where

import List
import Types

rhythmChance :: ChanceType
rhythmChance now past = foldl (*) 1
	(map (\(chance, factor) -> (** factor) $ chance now past) chances)

chances :: [(ChanceType, Float)]
chances = [
	(chanceMeasureTime, 1),
	(chanceBeatTime, 1),
	(chanceCopyRhythm, 0.8)
	]

chanceMeasureTime now _ = if (remain1 - dur1 >= 0) then 1 else floatZero
	where dur1 = dur now; remain1 = remain now

chanceBeatTime now _ =
	if (dur1 > beat || (remain1 - dur1) `mod` beat > 0) then floatHalf else 1
	where dur1 = dur now; remain1 = remain now;
		beat = measure now `div` beats now

chanceCopyRhythm now flow =
	if prems == [] || elem remain1 prems then 1 else floatHalf where
		remain1 = remain now
		prems = measureRems (now:flow) 1
		measureRems :: Flow -> Int -> [Duration]
		measureRems flow skip
			| flow == [] = []
			| skip > 0 && measure1 <= remain1 = measureRems before (skip - 1)
			| skip > 0 && measure1 > remain1 = measureRems before skip
			| skip == 0 && measure1 <= remain1 = [remain1]
			| skip == 0 && measure1 > remain1 = remain1 : measureRems before 0
			where (ch:before) = flow; measure1 = measure ch; remain1 = remain ch

