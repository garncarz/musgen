module InterpretationChurch (churchTracks) where

import InterpretationTechniques
import Midi
import Relations
import Types

tracks :: TrackDefs
tracks = [
	(1, "Harmony", 19, harmonyTrack),
	(2, "Soprano", 1, sopranoTrack),
	(3, "Bass", 33, bassTrack),
	(4, "Rhythm", 26, harmonyRhythmTrack)
	]

churchTracks :: TracksGen
churchTracks = makeTracks tracks

harmonyTrack flow _ = flow2Midi flow

sopranoTrack flow gen = (flow2Midi . melodyFlow gen) (takePart flow) where
	takePart [] = []
	takePart flow = ch2 : takePart rest	where
		(ch:rest) = flow; tone1 = maximum $ tones ch; ch2 = ch {tones = [tone1]}

bassTrack flow _ = (flow2Midi . fingeredFlow) (lowerFlow flow) where
	lowerFlow = map (\ch -> ch {tones = map (subtract (scaleSize * 2)) $
		tones ch})

harmonyRhythmTrack flow _ = (flow2Midi . chordRhythmFlow) flow

