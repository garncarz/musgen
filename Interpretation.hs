module Interpretation (interpretations) where

import InterpretationTechniques
import Midi
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


harmonyTrack flow _ = flow2Midi flow

sopranoTrack flow gen = (flow2Midi . sopranoFlow gen) (takePart flow) where
	takePart [] = []
	takePart flow = ch2 : takePart rest	where
		(ch:rest) = flow; tone1 = maximum $ tones ch; ch2 = ch {tones = [tone1]}

bassTrack flow _ = (flow2Midi . fingeredFlow) (octaveShift (-2) flow)

harmonyRhythmTrack flow _ = (flow2Midi . chordRhythmFlow) flow

harmonyRhythmTrackRock flow _ = (flow2Midi . chordRhythmFlow . octaveShift (-1))
	flow

additionTrack flow gen = (flow2Midi . randomMelodyFlow gen) flow

