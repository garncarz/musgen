module ChanceHarmony (harmonyChance) where

import List
import Types
import Relations

type ChanceType = Chord -> [Chord] -> MusicState -> Float

harmonyChance :: ChanceType
harmonyChance ch past st = foldl (*) 1
	(map (\(chance, factor) -> (** factor) $ chance ch past st) chances)

chances :: [(ChanceType, Float)]
chances = [
	(chanceLeadingTone, 1),
	(chanceTonicStart, 1),
	(chanceNotDomThenSub, 1),
	(chanceInScale, 0.5),
	(chanceThick, 0.1),
	(chanceJumps, 0),
	(chanceTriad, 0.8),
	(chanceNotEmpty, 0.9)
	--(chance4Tones, 0.8)
	]

chanceThick tones _ st = recip $ sum $
	1 : map (\t -> (/ 20) $ fromIntegral . abs $ t - (base st)) tones

chanceJumps _ [] _ = 1
chanceJumps tones (ptones:_) _ = recip $ sum $
	1 : map (\t -> if isToneJump t ptones then 1 else 0) tones

chanceInScale tones _ st = if all (\t -> isToneFromScale t (base st)
	(intervals st)) tones then 1 else floatMin

chanceTriad tones _ st = if null tones ||
	isFullTriad tones (base st) (intervals st) then 1 else floatMin

chanceTonicStart tones [] st =
	if isTonic tones (base st) (intervals st) then 1 else 0
chanceTonicStart _ _ _ = 1

chanceNotDomThenSub tones (ptones:_) st =
	if isDominant ptones (base st) (intervals st) &&
		isSubdominant tones (base st) (intervals st)
		then floatZero else 1
chanceNotDomThenSub _ [] _ = 1

chanceLeadingTone tones (ptones:_) st =
	if isLeadingToneOk ptones tones (base st) (intervals st) then 1 else 0
chanceLeadingTone tones [] st =
	if isLeadingToneOk [] tones (base st) (intervals st) then 1 else 0

chanceNotEmpty [] _ _ = floatMin
chanceNotEmpty _ _ _ = 1

chance4Tones tones _ _ =
	if length tones == 4 then 1 else floatMin

