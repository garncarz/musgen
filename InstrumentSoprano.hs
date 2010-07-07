module InstrumentSoprano (sopranoTrack) where

import List
import Midi
import Types

takePart :: Flow -> [ToneToStop] -> [MidiEvent]
takePart flow playing
	| plToneEnds = toneMidi plTone 0 : takePart flow plRest
	| toneStarts = toneMidi tone 90 : pauseMidi pause :
		takePart fRest newPlaying
	| plToneContinues = pauseMidi pause : takePart fRest newPlaying
	| otherwise = []
	where
		isPlaying = length playing > 0
		(plFstEnding:plRest) = playing
		(plTone, plDur) = plFstEnding
		plTones = map (\(t, _) -> t) playing
		
		isFlow = length flow > 0
		((ch, dur):fRest) = flow
		tone = maximum ch
		
		toneStarts = isFlow && (not isPlaying || (not $ elem tone plTones))
		plToneEnds = isPlaying && plDur == 0 && (not isFlow || tone /= plTone)
		plToneContinues = isPlaying && plDur == 0 && isFlow && plTone == tone
		
		pause = minimum $ if isFlow then [dur] else []
			++ if isPlaying then [plDur] else []
		newPlaying = sortPlayingEnd $ map (\(t, d) -> (t, d - pause)) $
			(tone, dur) : (if plToneContinues then plRest else playing)
		
		sortPlayingEnd = sortBy (\(_, d1) (_, d2) -> compare d1 d2)

sopranoTrack :: Flow -> MusicState -> MidiTrack
sopranoTrack flow st = midiTrack 2 "Soprano" 74 (takePart flow [])
	(base st) (intervals st)

