module Chances where

import List
import Types
import Relations

type ChanceType = Chord -> Flow -> MusicState -> Float

chance :: ChanceType
chance ch past st = foldl (*) 1
	(map (\(chance, factor) -> (** factor) $ chance ch past st) chances)

chances :: [(ChanceType, Float)]
chances = [
	(chanceLeadingTone, 1),
	(chanceTonicStart, 1),
	(chanceNotDomThenSub, 1),
	(chanceThick, 0.1),
	(chanceFast, 0.5),
	(chanceJumps, 0),
	(chanceInScale, 0.9),
	(chanceTriad, 0.5),
	(chanceNotEmpty, 0.9),
	(chance4Tones, 0.8)
	]

chanceThick (tones, dur) _ (base, _) = recip $ sum $
	1 : map (\t -> (/ 20) $ fromIntegral . abs $ t - base) tones

chanceFast (_, dur) _ _ = minimum [5 / fromIntegral dur, 1]

chanceJumps _ [] _ = 1
chanceJumps (tones, _) ((ptones, _):_) _ = recip $ sum $
	1 : map (\t -> if isToneJump t ptones then 1 else 0) tones

chanceInScale (tones, _) _ (base, intervals) = recip $ sum $
	1 : map (\t -> if isToneFromScale t base intervals then 0 else 1) tones

chanceTriad (tones, _) _ (base, intervals) =
	if null tones || isFullTriad tones base intervals then 1 else 0.5

chanceTonicStart (tones, _) [] (base, intervals) =
	if isTonic tones base intervals then 1 else 0
chanceTonicStart _ _ _ = 1

chanceNotDomThenSub (tones, _) ((ptones, _):_) (base, intervals) =
	if isDominant ptones base intervals && isSubdominant tones base intervals
		then 0 else 1
chanceNotDomThenSub _ [] _ = 1

chanceLeadingTone (tones, _) ((ptones, _):_) (base, intervals) =
	if isLeadingToneOk ptones tones base intervals then 1 else 0
chanceLeadingTone (tones, _) [] (base, intervals) =
	if isLeadingToneOk [] tones base intervals then 1 else 0

chanceNotEmpty ([], _) _ _ = 0.1
chanceNotEmpty _ _ _ = 1

chance4Tones (tones, _) _ _ =
	if length tones == 4 then 1 else 0.5

