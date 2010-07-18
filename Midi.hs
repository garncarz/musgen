module Midi (toneMidi, pauseMidi, midiTrack, midiFile, exportMidi) where

import Codec.Midi
import Relations
import Types hiding (key)

toneMidi :: Tone -> Volume -> MidiEvent
toneMidi t vol = (0, NoteOn {channel = 0, key = t, velocity = vol})

pauseMidi :: Int -> MidiEvent
--pauseMidi dur = (dur, Marker "")
pauseMidi dur = (dur, NoteOff {channel = 0, key = 0, velocity = 0})

keySignature :: Tone -> Intervals -> (Int, Int)
keySignature key intervals
	| t == 0 && mjr || t == 9 && not mjr = (0, mjrI)
	| otherwise = (\(sharps, mjrI) -> (nextSharps sharps, mjrI))
		(keySignature (key - 7) intervals)
	where
		t = intervalFromTo 60 key
		mjr = intervals == major
		mjrI = if mjr then 0 else 1
		nextSharps sharps = if sharps == 6 then -5 else sharps + 1

eventsToChannel :: Channel -> [MidiEvent] -> [MidiEvent]
eventsToChannel chan = map (\(ticks, msg) -> if isNoteOn msg
	then let msgUpdated = NoteOn {channel = chan, key = key msg,
		velocity = velocity msg} in (ticks, msgUpdated)
	else (ticks, msg))

midiTrack :: Int -> String -> Int -> [MidiEvent] -> Tone -> Intervals ->
	MidiTrack
midiTrack channel instrument program events key intervals =
	[
		(0, ChannelPrefix channel),
		(0, InstrumentName instrument),
		(0, ProgramChange channel program),
		--(0, TimeSignature ),
		(0, (\(x, y) -> KeySignature x y) (keySignature key intervals))
	]
	++ (eventsToChannel channel events) ++
	[ (5, TrackEnd) ]

midiFile :: [MidiTrack] -> Midi
midiFile tracks = Midi {
	fileType = MultiTrack,
	timeDiv = TicksPerBeat 4,
	tracks = tracks }

exportMidi :: String -> Midi -> IO()
exportMidi filename midi = exportFile filename midi

