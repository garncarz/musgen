module Types where

import Codec.Midi
import Random

type Tone = Int
type Interval = Int
type Intervals = [Interval]
type Volume = Int
type Duration = Int

data Chord = Chord {
	tones :: [Tone],
	key :: Tone,
	intervals :: Intervals,
	dur :: Duration,
	measure :: Duration,
	remain :: Duration,
	beats :: Int} deriving (Eq, Show, Read)
showBrief :: Chord -> String
--showBrief ch = show (tones ch, dur ch, remain ch)
showBrief ch = "(" ++ show (tones ch) ++ "," ++ show (dur ch) ++ "," ++
	show (remain ch) ++ "->)"
type Flow = [Chord]

type ChanceType = Chord -> Flow -> Float

type MidiEvent = (Ticks, Message)
type MidiTrack = [MidiEvent]
type InstrumentTrack = Flow -> StdGen -> Tempo -> MidiTrack


floatMin = 0.1 :: Float
floatZero = 0.001 :: Float
floatHalf = 0.5 + floatMin

