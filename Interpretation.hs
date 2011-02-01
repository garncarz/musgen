module Interpretation (interpretations) where

import InterpretationTechniques
import Midi
import MGRandom
import Random
import Types

interpretations :: [(String, TracksDefs)]
interpretations = [
	("church", churchTracks),
	("pop", popTracks),
	("rock", rockTracks)
	]

churchTracks = [
	(1, "Harmony", 19, 0.7, harmonyTrack),
	(2, "Soprano", 1, 1, sopranoTrack),
	(3, "Bass", 33, 1, bassTrack),
	(4, "Rhythm", 26, 0.7, harmonyRhythmTrack),
	(5, "Addition", 7, 1, additionTrack)
	]

popTracks = [
	(1, "Harmony", 51, 0.5, harmonyTrack),
	(2, "Melody", 81, 1, sopranoTrack),
	(3, "Bass", 39, 1, bassTrack),
	(4, "Rhythm", 26, 0.5, harmonyRhythmTrack),
	(5, "Addition", 82, 0.8, additionTrack)
	]

rockTracks = [
	(1, "Harmony", 31, 0.7, harmonyTrack),
	(2, "Melody", 30, 1, sopranoTrack),
	(3, "Bass", 34, 1, bassTrack),
	(4, "Rhythm", 30, 0.7, harmonyRhythmTrackRock),
	(5, "Addition", 7, 1, additionTrack)
	]


harmonyTrack flow _ = flow

sopranoTrack flow gen = sopranoFlow gen flow

--bassTrack flow gen = fingeredFlow gen $ octaveShift (-2) flow
--bassTrack flow gen = walkingBass gen (octaveShift (-2) flow)

--harmonyRhythmTrack flow gen = chordRhythmFlow gen flow
harmonyRhythmTrack flow gen = brokenChord1_5_10 gen flow

harmonyRhythmTrackRock flow gen = chordRhythmFlow gen (octaveShift (-1) flow)

additionTrack flow gen = randomMelodyFlow gen flow


bassTrack flow gen = switchBassInner flow gen 0 where
	switchBassInner [] _ _ = []
	switchBassInner flow gen track = flow1 ++
		switchBassInner (flow2 flow $ sumDur flow1) (genL !! 2) track
		where
		flow1 = (pFlow1 !! 0) : takeWhile (\ch -> begin ch /= 0)
			[pFlow1 !! i | i <- [1..length pFlow1 - 1]]
		pFlow1 = ([fingeredFlow, walkingBass] !! alg) (genL !! 1)
			(octaveShift (-2) flow)
		(alg, _) = randomR (0 :: Int, 1) $ genL !! 0
		
		sumDur :: Flow -> Int
		sumDur [] = 0
		sumDur flow = dur ch + sumDur rest where ch:rest = flow
		
		flow2 :: Flow -> Int -> Flow
		flow2 [] _ = []
		flow2 flow sum
			| sum <= 0 = flow
			| otherwise = flow2 (tail flow) (sum - dur (flow !! 0))
		
		genL = rndSplitL gen

