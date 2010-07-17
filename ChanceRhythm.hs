module ChanceRhythm (rhythmChance) where

import List
import Types

type ChanceType = TimedChord -> [TimedChord] -> MusicState -> Float

rhythmChance :: ChanceType
rhythmChance ch past st = foldl (*) 1
	(map (\(chance, factor) -> (** factor) $ chance ch past st) chances)

chances :: [(ChanceType, Float)]
chances = [
	(chanceBeatTime, 1),
	(chanceRightSpeed, 0.5),
	(chanceSomeRhythm, 0.5),
	(chanceCopyRhythm, 0.1)
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
chanceSomeRhythm (_, dur) [] _ = 1

chanceCopyRhythm ch flow st =
	if past == [] || isPrefixOf actual past then 1 else floatMin
	where
		actual = rhythmHead (ch:flow) (beat st) (beat st - remain st + dur) 0
		past = rhythmHead (ch:flow) (beat st) (beat st - remain st + dur) 1
		(_, dur) = ch
		rhythmHead :: [TimedChord] -> Duration -> Duration -> Int -> [Duration]
		rhythmHead flow beat passed skip
			| passed < 0 || flow == [] = []
			| passed == 0 && skip <= 0 = []
			| passed == 0 = rhythmHead flow beat beat (skip - 1)
			| passed > 0 && skip > 0 = rhythmHead rest beat (passed - dur) skip
			| otherwise = (rhythmHead rest beat (passed - dur) skip) ++ [dur]
			where
				((_, dur):rest) = flow

