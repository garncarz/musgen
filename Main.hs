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
		key = let (i, _) = randomR (-6, 6) (g !! 0) in i + 64
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
		ch1 = pch {tones = tones, remain = nextRemain}
		tones = nextTones past (g !! 0)
		nextRemain = let diff = remain pch - Types.dur pch in if diff > 0
			then diff else measure pch
		endCh = ch1 {dur = remain ch1}
		isEnd = canBeEnd endCh rpast
		dur = nextDur ch1 past (g !! 1)
		ch = ch1 {dur = dur}
		rpast = realPast past
		(pch:prest) = past
		g = rndSplitL gen

nextTones :: RandomGen g => Flow -> g -> [Tone]
nextTones past gen = if ok then tones else nextTones past (g !! 2) where
	ok = chance > rndChance
	chance = harmonyChance ch rpast
	ch = pch {tones = tones, dur = 0}
	tones = rndChordTones (g !! 0)
	rpast = realPast past
	(pch:prest) = past
	g = rndSplitL gen
	(rndChance, _) = randomR (0.4 :: Float, 1) (g !! 1)

nextDur :: RandomGen g => Chord -> Flow -> g -> Duration
nextDur ch past gen = if ok then dur else nextDur ch past (g !! 2) where
	ok = chance > rndChance
	chance = rhythmChance ch1 rpast
	ch1 = ch {dur = dur}
	dur = rndDuration (g !! 0)
	rpast = realPast past
	(pch:prest) = past
	g = rndSplitL gen
	(rndChance, _) = randomR (0.4 :: Float, 1) (g !! 1)

canBeEnd :: Chord -> Flow -> Bool
canBeEnd ch past = length past > 40 && isTonicTriadIn key1 intervals1 tones1 &&
	2 * dur1 >= measure1
	where tones1 = tones ch; key1 = key ch; intervals1 = intervals ch;
		dur1 = dur ch; measure1 = measure ch

realPast :: Flow -> Flow
realPast past = if (dur pch == 0 && tones pch == []) then [] else past
	where (pch:_) = past

