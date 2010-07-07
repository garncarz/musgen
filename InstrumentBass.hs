module InstrumentBass (bassTrack) where

import List
import Midi
import Types

takePart :: Flow -> [ToneToStop] -> [MidiEvent]
takePart flow playing
	| plToneEnds = toneMidi plTone 0 : takePart flow plRest
	| toneStarts = toneMidi tone 80 : pauseMidi pause :
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
		tone = minimum ch
		
		toneStarts = isFlow && (not isPlaying || (not $ elem tone plTones))
		plToneEnds = isPlaying && plDur == 0 && (not isFlow || tone /= plTone)
		plToneContinues = isPlaying && plDur == 0 && isFlow && plTone == tone
		
		pause = minimum $ if isFlow then [dur] else []
			++ if isPlaying then [plDur] else []
		newPlaying = sortPlayingEnd $ map (\(t, d) -> (t, d - pause)) $
			(tone, dur) : (if plToneContinues then plRest else playing)
		
		sortPlayingEnd = sortBy (\(_, d1) (_, d2) -> compare d1 d2)

bassTrack :: Flow -> MusicState -> MidiTrack
bassTrack flow st = midiTrack 3 "Bass" 33 (takePart bassFlow [])
	(base st) (intervals st) where
		bassFlow = map (\(ch, dur) -> (map (\t -> t - 12) ch, dur)) flow

