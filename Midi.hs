module Midi (exportFlow) where

import Codec.Midi
import Relations
import Types

toneMidi :: Tone -> Volume -> MidiEvent
toneMidi t vol = (0, NoteOn {channel = 0, key = t, velocity = vol})

chordMidi :: TimedChord -> [MidiEvent]
chordMidi ([], dur) = [(dur, Marker "")]
chordMidi ((t:ts), dur) = [(toneMidi t 80)] ++ (chordMidi (ts, dur))
	++ [(toneMidi t 0)]

keySignature :: Tone -> Intervals -> (Int, Int)
keySignature base i
	| t == 0 && mjr || t == 9 && not mjr = (0, mjrI)
	| t == 7 && mjr || t == 4 && not mjr = (1, mjrI)
	| t == 2 && mjr || t == 11 && not mjr = (2, mjrI)
	| t == 9 && mjr || t == 6 && not mjr = (3, mjrI)
	| t == 4 && mjr || t == 1 && not mjr = (4, mjrI)
	| t == 11 && mjr || t == 8 && not mjr = (5, mjrI)
	| t == 6 && mjr || t == 3 && not mjr = (6, mjrI)
	| t == 5 && mjr || t == 2 && not mjr = (-1, mjrI)
	| t == 10 && mjr || t == 7 && not mjr = (-2, mjrI)
	| t == 3 && mjr || t == 0 && not mjr = (-3, mjrI)
	| t == 8 && mjr || t == 5 && not mjr = (-4, mjrI)
	| otherwise = (0, 0)
	where
		t = intervalFromTo 60 base
		mjr = i == major
		mjrI = if mjr then 0 else 1

midiFile :: [MidiEvent] -> Tone -> Intervals -> Midi
midiFile events base intervals = Midi {
	fileType = MultiTrack,
	timeDiv = TicksPerBeat 4,
	tracks =
		[
			[
				(0, ChannelPrefix 0),
				(0, InstrumentName "Harpsichord"),
				(0, ProgramChange 0 6),
				(0, (\(x, y) -> KeySignature x y) (keySignature base intervals))
			]
			++ events ++
			[
				(0, TrackEnd)
			]
		]
	}

exportFlow :: Flow -> MusicState -> IO()
exportFlow flow st = exportFile "export.midi" $ midiFile (concat $ map chordMidi
	flow) (base st) (intervals st)

