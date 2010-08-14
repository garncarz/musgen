module InstrumentHarmony (harmonyTrack) where

import Midi
import Random
import Types

chordMidi :: [Tone] -> Duration -> [MidiEvent]
chordMidi [] dur = [pauseMidi dur]
chordMidi (t:ts) dur = [toneMidi t 80] ++ (chordMidi ts dur) ++ [toneMidi t 0]

harmonyChord :: Chord -> [MidiEvent]
harmonyChord chord = chordMidi tones1 dur1 where
	tones1 = tones chord; dur1 = dur chord
	measure1 = measure chord; remain1 = remain chord
	key1 = key chord; intervals1 = intervals chord

harmonyTrack :: RandomGen g => Flow -> g -> MidiTrack
harmonyTrack flow _ = midiTrack 1 "Harmony" 19 (concat $ map harmonyChord flow)
	(flow !! 0)

