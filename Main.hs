module Main where

import Random

import ChanceHarmony
import ChanceRhythm
import InstrumentBass
import InstrumentHarmony
import InstrumentSoprano
import Midi
import MGRandom
import Relations
import Types

main :: IO()
main = do
	gen <- newStdGen
	let
		g = rndSplitL gen
		key = let (i, _) = randomR (-6, 6) (g !! 0) in i + 76
		(mjr, _) = randomR (True, False) (g !! 1)
		startState = Chord {tones = [], key = key,
			intervals = if mjr then major else minor,
			dur = 0, measure = 16, remain = 16, beats = 4}
		flow = generateFlow [startState] (g !! 2)
	mapM_ putStrLn $ map show flow
	exportMidi "song.midi" $ midiFile [harmonyTrack flow, sopranoTrack flow,
		bassTrack flow]

generateFlow :: RandomGen g => Flow -> g -> Flow
generateFlow past gen =
	if isEnd then [endCh] else ch : generateFlow (ch:rpast) (g !! 2)
	where
		ch1 = nextTonesChord past (g !! 0)
		endCh = ch1 {dur = remain ch1}; isEnd = canBeEnd endCh rpast
		ch = nextDurChord ch1 past (g !! 1)
		rpast = realPast past; (pch:_) = past
		g = rndSplitL gen

nextTonesChord :: RandomGen g => Flow -> g -> Chord
nextTonesChord past gen = if ok then ch else nextTonesChord past (g !! 2) where
	ok = chance >= minChance; chance = harmonyChance ch rpast
	ch = pch {tones = rndChordTones (g !! 0), dur = 0, remain = newRemain}
	newRemain = let diff = remain pch - Types.dur pch in if diff > 0
			then diff else measure pch
	rpast = realPast past; (pch:_) = past
	g = rndSplitL gen
	(minChance, _) = randomR (0.5 :: Float, 0.7) (g !! 1)

nextDurChord :: RandomGen g => Chord -> Flow -> g -> Chord
nextDurChord ch past gen = if ok then ch1 else nextDurChord ch past (g !! 2)
	where
		ok = chance >= minChance; chance = rhythmChance ch1 rpast
		ch1 = ch {dur = rndDuration (g !! 0)}
		rpast = realPast past
		g = rndSplitL gen
		(minChance, _) = randomR (0.5 :: Float, 0.7) (g !! 1)

canBeEnd :: Chord -> Flow -> Bool
canBeEnd ch past = length past > 40 && isTonicTriadIn key1 intervals1 tones1 &&
	2 * dur1 >= measure1
	where tones1 = tones ch; key1 = key ch; intervals1 = intervals ch;
		dur1 = dur ch; measure1 = measure ch

realPast :: Flow -> Flow
realPast past = if (dur pch == 0 && tones pch == []) then [] else past
	where (pch:_) = past

