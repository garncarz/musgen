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
	begin :: Duration,
	dur :: Duration,
	measure :: Duration,
	beats :: Int } deriving (Eq, Show, Read)
remain :: Chord -> Duration
remain ch = measure ch - begin ch
showBrief :: Chord -> String
--showBrief ch = show (tones ch, dur ch, remain ch)
showBrief ch = "(" ++ show (tones ch) ++ ",b" ++ show (begin ch) ++ ",d" ++
	show (dur ch) ++ ",r" ++ show (remain ch) ++ ")"
type Flow = [Chord]

type ChanceType = Chord -> Flow -> Float

type MidiEvent = (Ticks, Message)
type MidiTrack = [MidiEvent]
type TracksDefs = [(Channel, String, Preset, Float,
	Flow -> RndGen -> Flow)]

type RndGen = StdGen

floatMin = 0.1 :: Float
floatZero = 0.001 :: Float
floatHalf = 0.5 + floatMin

