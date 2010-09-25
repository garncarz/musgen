module Interpretation (interpretations) where

import InterpretationTechniques
import Midi
import Types

interpretations :: [(String, TracksDefs)]
interpretations = [
	("church", churchTracks),
	("rock", rockTracks)
	]

churchTracks = [
	(1, "Harmony", 19, harmonyTrack),
	(2, "Soprano", 1, sopranoTrack),
	(3, "Bass", 33, bassTrack),
	(4, "Rhythm", 26, harmonyRhythmTrack),
	(5, "Addition", 7, additionTrack)
	]

rockTracks = [
	(1, "Harmony", 31, harmonyTrack),
	(2, "Melody", 30, sopranoTrack),
	(3, "Bass", 34, bassTrack),
	(4, "Rhythm", 30, harmonyRhythmTrackRock),
	(5, "Addition", 7, additionTrack)
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

