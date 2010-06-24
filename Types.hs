module Types where

import Codec.Midi

type Tone = Int
type Intervals = [Int]
type Volume = Int
type Duration = Int
type Chord = ([Tone], Duration)
type Flow = [Chord]
type MusicState = (Tone, Intervals)

type MidiEvent = (Ticks, Message)

