module Midi (toneMidi, pauseMidi, midiTrack, midiFile, exportMidi) where

import Codec.Midi
import Relations
import Types hiding (key)
import qualified Types (key)

toneMidi :: Tone -> Volume -> MidiEvent
toneMidi t vol = (0, NoteOn {channel = 0, key = t, velocity = vol})

pauseMidi :: Int -> MidiEvent
--pauseMidi dur = (dur, Marker "")
pauseMidi dur = (dur, NoteOff {channel = 0, key = 0, velocity = 0})

keySignature :: Tone -> Intervals -> Message
keySignature key intervals
	| t == 0 && mjr || t == 9 && not mjr = KeySignature 0 mjrI
	| otherwise = KeySignature (nextSharps sharps2) mjrI2
	where
		t = intervalFromTo 60 key
		mjr = intervals == major
		mjrI = if mjr then 0 else 1
		nextSharps sharps = if sharps == 6 then -5 else sharps + 1
		KeySignature sharps2 mjrI2 = keySignature (key - 7) intervals

timeSignature :: Duration -> Int -> Message
timeSignature measure beats = TimeSignature numerator denominator clocks dunno
	where numerator = beats; denominator = 2; clocks = 18; dunno = 8

eventsToChannel :: Channel -> [MidiEvent] -> [MidiEvent]
eventsToChannel chan = map (\(ticks, msg) -> if isNoteOn msg
	then (ticks, msg {channel = chan})
	else (ticks, msg))

midiTrack :: Int -> String -> Int -> [MidiEvent] -> Chord -> MidiTrack
midiTrack channel instrument program events state =
	[
		(0, ChannelPrefix channel),
		(0, InstrumentName instrument),
		(0, ProgramChange channel program),
		(0, keySignature (Types.key state) (intervals state)),
		(0, timeSignature (measure state) (beats state))
	]
	++ (eventsToChannel channel events) ++
	[ (8, TrackEnd) ]

midiFile :: [MidiTrack] -> Midi
midiFile tracks = Midi {
	fileType = MultiTrack,
	timeDiv = TicksPerBeat 4,
	tracks = tracks }

exportMidi :: String -> Midi -> IO()
exportMidi filename midi = exportFile filename midi

