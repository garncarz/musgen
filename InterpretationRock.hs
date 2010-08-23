module InterpretationRock (rockTracks) where

import InterpretationTechniques
import Midi
import Relations
import Types

tracks :: TrackDefs
tracks = [
	(1, "Harmony", 31, harmonyTrack),
	(2, "Melody", 30, melodyTrack),
	(3, "Bass", 34, bassTrack),
	(4, "Rhythm", 30, harmonyRhythmTrack)
	]

rockTracks :: TracksGen
rockTracks = makeTracks tracks

harmonyTrack flow _ = flow2Midi flow

melodyTrack flow gen = (flow2Midi . melodyFlow gen) (takePart flow) where
	takePart [] = []
	takePart flow = ch2 : takePart rest	where
		(ch:rest) = flow; tone1 = maximum $ tones ch; ch2 = ch {tones = [tone1]}

bassTrack flow _ = (flow2Midi . fingeredFlow) (octaveShift (-2) flow)

harmonyRhythmTrack flow _ = (flow2Midi . chordRhythmFlow . octaveShift (-1))
	flow

