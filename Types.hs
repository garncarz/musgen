module Types where

import Codec.Midi

type Tone = Int
type Intervals = [Int]
type Volume = Int
type Duration = Int
type BeatTime = Int

type Chord = [Tone]
type TimedChord = (Chord, Duration)
type Flow = [TimedChord]

data MusicState = MusicState {
	base :: Tone,
	intervals :: Intervals,
	beat :: BeatTime,
	remain :: BeatTime}

type MidiEvent = (Ticks, Message)


floatMin = 0.1 :: Float
floatZero = 0.001 :: Float

nullChord :: TimedChord
nullChord = ([], 0)

