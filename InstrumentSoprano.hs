module InstrumentSoprano (sopranoTrack) where

import List
import Midi
import MGRandom
import Random
import Relations
import Types

takePart :: Flow -> Flow
takePart [] = []
takePart flow = ch2 : takePart rest
	where
		(ch:rest) = flow
		tone1 = maximum $ tones ch
		ch2 = ch {tones = [tone1]}

melodyFlow :: RandomGen g => g -> Flow -> Flow
melodyFlow _ [] = []
melodyFlow gen flow = chs ++ melodyFlow (g !! 0) rest
	where
		(ch1:rest) = flow; tone1 = head $ tones ch1; dur1 = dur ch1
		key1 = key ch1; intervals1 = intervals ch1
		(ch2:_) = rest; tone2 = head $ tones ch2
		
		tone1Up = succToneIn key1 intervals1 tone1
		tone1Down = predToneIn key1 intervals1 tone1
		tone2Up = succToneIn key1 intervals1 tone2
		tone2Down = predToneIn key1 intervals1 tone2
		
		tone1UpLead = isLeadingToneIn key1 intervals1 tone1Up
		tone1DownLead = isLeadingToneIn key1 intervals1 tone1Down
		
		chs = if dur1 > 4 && not tone1UpLead && (yes !! 0) then [
			ch1 {tones = [tone1], dur = dur1 `div` 3},
			ch1 {tones = [tone1Up], dur = dur1 `div` 3},
			ch1 {tones = [tone1], dur = dur1 - 2 * (dur1 `div` 3)}]
			else if dur1 > 4 && (yes !! 1) then [
			ch1 {tones = [tone1], dur = dur1 `div` 3},
			ch1 {tones = [tone1Down], dur = dur1 `div` 3},
			ch1 {tones = [tone1], dur = dur1 - 2 * (dur1 `div` 3)}]
			else if length flow > 1 && tone1Up == tone2Down && (yes !! 2) then [
			ch1 {tones = [tone1], dur = dur1 `div` 2},
			ch1 {tones = [tone1Up], dur = dur1 - (dur1 `div` 2)}]
			else if length flow > 1 && tone1Down == tone2Up && not tone1DownLead
			&& (yes !! 3) then [
			ch1 {tones = [tone1], dur = dur1 `div` 2},
			ch1 {tones = [tone1Down], dur = dur1 - (dur1 `div` 2)}]
			else [ch1]
		
		yes = randomRs (True, False) (g !! 1)
		g = rndSplitL gen

flowMidi :: Flow -> [MidiEvent]
flowMidi [] = []
flowMidi flow = startTones ++ [pauseMidi dur1] ++ endTones ++ flowMidi rest
	where
		(ch:rest) = flow; dur1 = dur ch
		startTones = map (\t -> toneMidi t 90) $ tones ch
		endTones = map (\t -> toneMidi t 0) $ tones ch

sopranoTrack :: RandomGen g => Flow -> g -> MidiTrack
sopranoTrack flow gen = midiTrack 2 "Soprano" 1
	(flowMidi $ melodyFlow gen $ takePart flow) (flow !! 0)

