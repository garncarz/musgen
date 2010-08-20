module ChanceHarmonyRhythm (harmonyRhythmChance) where

import Data.List
import Types

harmonyRhythmChance :: ChanceType
harmonyRhythmChance now past = product
	(map (\(chance, factor) -> (** factor) $ chance now past) chances)

chances :: [(ChanceType, Float)]
chances = [
	(chanceMeasureTime, 1),
	(chanceBeatTime, 0.8)
	]

chanceMeasureTime now _ = if remain1 - dur1 >= 0 then 1 else floatZero
	where dur1 = dur now; remain1 = remain now

chanceBeatTime now flow
	| remain1 - dur1 `mod` beat == 0 = 1
	| otherwise = chanceCopyRhythm now flow
	where dur1 = dur now; remain1 = remain now;
		beat = measure now `div` beats now

chanceCopyRhythm now flow = if prems == [] || nextRemain == 0 ||
	elem nextRemain prems then 1 else floatHalf where
		nextRemain = remain now - dur now
		prems = measureRems (now:flow) 1
		measureRems :: Flow -> Int -> [Duration]
		measureRems flow skip
			| flow == [] = []
			| skip > 0 && measure1 <= remain1 = measureRems before (skip - 1)
			| skip > 0 && measure1 > remain1 = measureRems before skip
			| skip == 0 && measure1 <= remain1 = [remain1]
			| skip == 0 && measure1 > remain1 = remain1 : measureRems before 0
			where (ch:before) = flow; measure1 = measure ch; remain1 = remain ch

