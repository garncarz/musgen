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
	(chanceThick, 0.15),
	(chanceFast, 0.1),
	(chanceJumps, 0),
	(chanceInScale, 0.9),
	(chanceTriad, 0.9),
	(chanceEmpty, 0.9),
	(chance4Tones, 0.8)
	]

chanceThick (tones, dur) _ (base, _) = recip $ sum $
	1 : map (\t -> fromIntegral . abs $ t - base) tones

chanceFast (_, dur) _ _ = minimum [5 / fromIntegral dur, 1]

chanceJumps _ [] _ = 1
chanceJumps (tones, _) ((ptones, _):_) _ = recip $ sum $
	1 : map (\t -> if isToneJump t ptones then 1 else 0) tones

chanceInScale (tones, _) _ (base, intervals) = recip $ sum $
	1 : map (\t -> if isToneFromScale t base intervals then 0 else 1) tones

chanceTriad (tones, _) _ (base, intervals) =
	if null tones || isChordTriad tones base intervals then 1 else 0.5

chanceEmpty ([], _) _ _ = 0
chanceEmpty _ _ _ = 1

chance4Tones (tones, _) _ _ =
	if length tones == 4 then 1 else 0

