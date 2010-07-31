module InstrumentSoprano (sopranoTrack) where

import List
import Midi
import Types

takePart :: Flow -> [ToneToStop] -> [MidiEvent]
takePart flow playing
	| plToneEnds = toneMidi plTone 0 : takePart flow plRest
	| toneStarts = toneMidi tone1 90 : pauseMidi pause :
		takePart fRest newPlaying
	| plToneContinues = pauseMidi pause : takePart fRest newPlaying
	| otherwise = []
	where
		isPlaying = length playing > 0
		(plFstEnding:plRest) = playing
		(plTone, plDur) = plFstEnding
		plTones = map (\(t, _) -> t) playing
		
		isFlow = length flow > 0
		(ch:fRest) = flow
		tone1 = maximum $ tones ch
		dur1 = dur ch
		
		toneStarts = isFlow && (not isPlaying || (not $ elem tone1 plTones))
		plToneEnds = isPlaying && plDur == 0 && (not isFlow || tone1 /= plTone)
		plToneContinues = isPlaying && plDur == 0 && isFlow && plTone == tone1
		
		pause = minimum $ if isFlow then [dur1] else []
			++ if isPlaying then [plDur] else []
		newPlaying = sortPlayingEnd $ map (\(t, d) -> (t, d - pause)) $
			(tone1, dur1) : (if plToneContinues then plRest else playing)
		
		sortPlayingEnd = sortBy (\(_, d1) (_, d2) -> compare d1 d2)

sopranoTrack :: Flow -> MidiTrack
sopranoTrack flow = midiTrack 2 "Soprano" 1 (takePart flow [])
	key1 intervals1
	where (ch:_) = flow; key1 = key ch; intervals1 = intervals ch

