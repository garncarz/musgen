module Midi (toneMidi, pauseMidi, flow2Midi,
	midiTrack, midiFile, exportMidi) where

import Codec.Midi
import Relations
import Types hiding (key)
import qualified Types (key)

toneMidi :: Tone -> Volume -> MidiEvent
toneMidi t vol = (0, NoteOn {channel = 0, key = t, velocity = vol})

pauseMidi :: Duration -> MidiEvent
--pauseMidi dur = (dur, Marker "")
pauseMidi dur = (dur, NoteOff {channel = 0, key = 0, velocity = 0})

flow2Midi :: Flow -> [MidiEvent]
flow2Midi [] = []
flow2Midi flow = startTones ++ [pauseMidi dur1] ++ endTones ++ flow2Midi rest
	where
		(ch:rest) = flow; dur1 = dur ch
		startTones = map (\t -> toneMidi t 90) $ tones ch
		endTones = map (\t -> toneMidi t 0) $ tones ch

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

timeSignature :: Int -> Message
timeSignature beats = TimeSignature beats 2 0 0

eventsToChannel :: Channel -> [MidiEvent] -> [MidiEvent]
eventsToChannel chan = map (\(ticks, msg) -> if isNoteOn msg
	then (ticks, msg {channel = chan})
	else (ticks, msg))

midiTrack :: Channel -> String -> Preset -> [MidiEvent] -> Chord -> Tempo
	-> MidiTrack
midiTrack channel name instrument events state tempo =
	[
		(0, ChannelPrefix channel),
		(0, InstrumentName name),
		(0, ProgramChange channel instrument),
		(0, keySignature (Types.key state) (intervals state)),
		(0, timeSignature (beats state)),
		(0, TempoChange tempo)
	]
	++ (eventsToChannel channel events) ++
	[ (8, TrackEnd) ]

midiFile :: [MidiTrack] -> Midi
midiFile tracks = Midi {
	fileType = MultiTrack,
	timeDiv = TicksPerBeat 4,
	tracks = tracks }

exportMidi :: String -> Midi -> IO ()
exportMidi filename midi = exportFile filename midi

