module InstrumentHarmonyRhythm (harmonyRhythmTrack) where

import Midi
import Random
import Types

chordMidi :: [Tone] -> Duration -> [MidiEvent]
chordMidi [] dur = [pauseMidi dur]
chordMidi (t:ts) dur = [toneMidi t 80] ++ chordMidi ts dur ++ [toneMidi t 0]

takePart :: Flow -> [MidiEvent]
takePart [] = []
takePart flow
	| dur1 <= 0 = error "dur1 <= 0"
	| overlap && dur2 >= dur1 = error "dur2 >= dur1"
	| otherwise = chordMidi tones1 dur2 ++ takePart flow2
	where
		remain1 = remain ch; dur1 = dur ch; tones1 = tones ch
		beat = measure ch `div` beats ch
		nextBeat = (remain1 - 1) `div` beat * beat
		(ch:rest) = flow
		remain2 = remain1 - dur1; overlap = remain2 < nextBeat
		flow2 = if overlap then ch2:rest else rest
		dur2 = if overlap then remain1 - nextBeat else dur1
		ch2 = if overlap then ch {dur = dur1 - dur2, remain = nextBeat} else ch

harmonyRhythmTrack :: InstrumentTrack
harmonyRhythmTrack flow _ = midiTrack 4 "Rhythm" 26 (takePart flow) (head flow)

