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
	| otherwise = (\(sharps, mjrI) -> (nextSharps sharps, mjrI))
		(keySignature (base - 7) i)
	where
		t = intervalFromTo 60 base
		mjr = i == major
		mjrI = if mjr then 0 else 1
		nextSharps sharps = if sharps == 6 then -5 else sharps + 1

midiFile :: [MidiEvent] -> Tone -> Intervals -> Midi
midiFile events base intervals = Midi {
	fileType = MultiTrack,
	timeDiv = TicksPerBeat 4,
	tracks =
		[
			[
				(0, ChannelPrefix 0),
				(0, InstrumentName "Piano"),
				(0, ProgramChange 0 0),
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

