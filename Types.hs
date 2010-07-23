module Types where

import Codec.Midi

type Tone = Int
type Interval = Int
type Intervals = [Interval]
type Volume = Int
type Duration = Int

type ToneToStop = (Tone, Duration)

data Chord =
	HarmonyChord {
		tones :: [Tone],
		key :: Tone,
		intervals :: Intervals}
	| TimedChord {
		tones :: [Tone],
		key :: Tone,
		intervals :: Intervals,
		dur :: Duration,
		beat :: Duration,
		remain :: Duration}
	deriving Eq
instance Show Chord where
	show ch = show (tones ch, dur ch)
type Flow = [Chord]

type ChanceType = Chord -> Flow -> Float

type MidiEvent = (Ticks, Message)
type MidiTrack = [MidiEvent]


floatMin = 0.1 :: Float
floatZero = 0.001 :: Float
floatHalf = 0.5 + floatMin

