module Types where

import Codec.Midi

type Tone = Int
type Volume = Int
type Duration = Int
type Chord = ([Tone], Duration)
type Flow = [Chord]

type MidiEvent = (Ticks, Message)

