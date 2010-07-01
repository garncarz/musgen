module Types where

import Codec.Midi

type Tone = Int
type Intervals = [Int]
type Volume = Int
type Duration = Int
type BeatTime = Int
type RemainingTime = Int
type Chord = ([Tone], Duration)
type Flow = [Chord]
data MusicState = MusicState {
	base :: Tone,
	intervals :: Intervals,
	beat :: BeatTime,
	remain :: RemainingTime}

type MidiEvent = (Ticks, Message)


nullChord :: Chord
nullChord = ([], 0)

