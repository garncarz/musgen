module InstrumentSoprano (sopranoTrack) where

import Interpretation
import Midi
import Random
import Types

takePart :: Flow -> Flow
takePart [] = []
takePart flow = ch2 : takePart rest
	where
		(ch:rest) = flow
		tone1 = maximum $ tones ch
		ch2 = ch {tones = [tone1]}

sopranoTrack :: InstrumentTrack
sopranoTrack flow gen = midiTrack 2 "Soprano" 1
	(flow2Midi . melodyFlow gen $ takePart flow) (head flow)

