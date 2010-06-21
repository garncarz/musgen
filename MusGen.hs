module MusGen where

import Codec.Midi
import Random


type Tone = Int
type Volume = Int
type Duration = Int
type Chord = ([Tone], Duration)
type Flow = [Chord]

type MidiEvent = (Ticks, Message)

toneMidi :: Tone -> Volume -> MidiEvent
toneMidi t vol = (0, NoteOn {channel = 0, key = t, velocity = vol})

chordMidi :: Chord -> [MidiEvent]
chordMidi ([], dur) = [(dur, Marker "time")]
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

cMaj :: Chord
cMaj = ([60, 64, 67], 16)


main :: IO()
main = do {
	newStdGen;
	gen <- getStdGen;
	let chords = [cMaj] ++ (take 10 $ rndChords gen)
	; exportFlow chords;
	print chords
}


rndChords :: RandomGen g => g -> [Chord]
rndChords g =
	let
		(tonesCount, g2) = randomR (1, 5) g
		(dur, g3) = randomR (1, 30) g2
		(g4, g5) = split g3
		tones = take tonesCount $ randomRs (0, 127) g4
	in (tones, dur) : rndChords g5

