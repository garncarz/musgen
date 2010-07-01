module Chances where

import List
import Types
import Relations

type ChanceType = Chord -> Flow -> MusicState -> Float

floatMin = 0.01 :: Float
floatZero = 0.001 :: Float

chance :: ChanceType
chance ch past st = foldl (*) 1
	(map (\(chance, factor) -> (** factor) $ chance ch past st) chances)

chances :: [(ChanceType, Float)]
chances = [
	(chanceLeadingTone, 1),
	(chanceTonicStart, 1),
	(chanceNotDomThenSub, 1),
	(chanceInScale, 0.9),
	(chanceBeatTime, 1),
	(chanceThick, 0.1),
	(chanceRightSpeed, 1),
	(chanceJumps, 0),
	(chanceTriad, 1),
	(chanceNotEmpty, 0.9),
	--(chance4Tones, 0.8),
	(chanceSomeRhythm, 0.5)
	]

chanceThick (tones, dur) _ st = recip $ sum $
	1 : map (\t -> (/ 20) $ fromIntegral . abs $ t - (base st)) tones

chanceRightSpeed (_, dur) _ _
	| dur == 8 = 1
	| dur == 4 = 0.8
	| dur == 16 = 0.8
	| dur == 2 = 0.4
	| otherwise = floatMin 

chanceJumps _ [] _ = 1
chanceJumps (tones, _) ((ptones, _):_) _ = recip $ sum $
	1 : map (\t -> if isToneJump t ptones then 1 else 0) tones

chanceInScale (tones, _) _ st = if foldl (\acc t -> (&&) acc $
	isToneFromScale t (base st) (intervals st)) True tones then 1 else floatMin

chanceTriad (tones, _) _ st = if null tones ||
	isFullTriad tones (base st) (intervals st) then 1 else floatMin

chanceTonicStart (tones, _) [] st =
	if isTonic tones (base st) (intervals st) then 1 else 0
chanceTonicStart _ _ _ = 1

chanceNotDomThenSub (tones, _) ((ptones, _):_) st =
	if isDominant ptones (base st) (intervals st) &&
		isSubdominant tones (base st) (intervals st)
		then floatZero else 1
chanceNotDomThenSub _ [] _ = 1

chanceLeadingTone (tones, _) ((ptones, _):_) st =
	if isLeadingToneOk ptones tones (base st) (intervals st)
		then 1
		else 0
chanceLeadingTone (tones, _) [] st =
	if isLeadingToneOk [] tones (base st) (intervals st) then 1 else 0

chanceBeatTime (_, dur) _ st = if (remain st - dur >= 0) then 1 else floatZero

chanceNotEmpty ([], _) _ _ = floatMin
chanceNotEmpty _ _ _ = 1

chance4Tones (tones, _) _ _ =
	if length tones == 4 then 1 else floatMin

chanceSomeRhythm (_, dur) ((_, pdur):_) _
	| dur == pdur = 0.9
	| dur == 2 * pdur = 1
	| pdur == 2 * dur = 1
	| otherwise = floatMin
chanceSomeRhythm (_, dur) [] _ = if dur == 8 then 1 else floatZero

