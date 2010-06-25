module Midi where

import Codec.Midi
import Types

toneMidi :: Tone -> Volume -> MidiEvent
toneMidi t vol = (0, NoteOn {channel = 0, key = t, velocity = vol})

chordMidi :: Chord -> [MidiEvent]
chordMidi ([], dur) = [(dur, Marker "")]
chordMidi ((t:ts), dur) = [(toneMidi t 80)] ++ (chordMidi (ts, dur))
	++ [(toneMidi t 0)]

midiFile :: [MidiEvent] -> Midi
midiFile events = Midi {
	fileType = MultiTrack,
	timeDiv = TicksPerBeat 8,
	tracks =
		[
			[
				(0, ChannelPrefix 0),
				(0, InstrumentName "Harpsichord"),
				(0, ProgramChange 0 6),
				(0, KeySignature 0 0)
			]
			++ events ++
			[
				(0, TrackEnd)
			]
		]
	}

exportFlow :: Flow -> IO()
exportFlow flow = exportFile "export.midi" $ midiFile $ concat $
	map chordMidi flow

