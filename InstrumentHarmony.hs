module InstrumentHarmony (harmonyTrack) where

import Midi
import Types

chordMidi :: [Tone] -> Duration -> [MidiEvent]
chordMidi [] dur = [pauseMidi dur]
chordMidi (t:ts) dur = [toneMidi t 80] ++ (chordMidi ts dur) ++ [toneMidi t 0]

harmonyTrack :: Flow -> MidiTrack
harmonyTrack flow = midiTrack 1 "Piano" 0 (concat $ map (\ch ->
	let tones1 = tones ch; dur1 = dur ch in chordMidi tones1 dur1)
	flow) key1 intervals1
	where (ch:_) = flow; key1 = key ch; intervals1 = intervals ch

