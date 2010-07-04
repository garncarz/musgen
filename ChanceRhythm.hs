module ChanceRhythm (rhythmChance) where

import Types

type ChanceType = TimedChord -> [TimedChord] -> MusicState -> Float

rhythmChance :: ChanceType
rhythmChance ch past st = foldl (*) 1
	(map (\(chance, factor) -> (** factor) $ chance ch past st) chances)

chances :: [(ChanceType, Float)]
chances = [
	(chanceBeatTime, 1),
	(chanceRightSpeed, 0.5),
	(chanceSomeRhythm, 0.5)
	]

chanceBeatTime (_, dur) _ st = if (remain st - dur >= 0) then 1 else floatZero

chanceRightSpeed (_, dur) _ _
	| dur == 8 = 1
	| dur == 4 = 0.8
	| dur == 16 = 0.8
	| dur == 2 = 0.4
	| otherwise = floatMin

chanceSomeRhythm (_, dur) ((_, pdur):_) st
	| beat st >= 4 * remain st = 1
	| dur == pdur = 0.9
	| dur == 2 * pdur = 1
	| pdur == 2 * dur = 1
	| otherwise = floatMin
chanceSomeRhythm (_, dur) [] _ = if dur == 8 then 1 else floatZero

