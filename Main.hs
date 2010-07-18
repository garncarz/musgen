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
		key = head $ rndTones (g !! 0)
		mjr = head $ randomRs (True, False) (g !! 1)
		startHarmony = HarmonyChord {tones = [], key = key,
			intervals = if mjr then major else minor}
		chordsSrc = rndChords startHarmony (g !! 2)
		harmonyFlow = createHarmonyFlow chordsSrc [] (g !! 3)
		startRhythm = TimedChord {tones = [], key = 0, intervals = [],
			dur = 0, beat = 16, remain = 16}
		dursSrc = rndDurations (g !! 4)
		flow = createTimedFlow harmonyFlow dursSrc [startRhythm] (g !! 5)
	mapM_ putStrLn $ map show flow
	--exportMidi "harmony.midi" $ midiFile [harmonyMidiFlow flow st]
	exportMidi "song.midi" $ midiFile [harmonyTrack flow, sopranoTrack flow,
		bassTrack flow]


createHarmonyFlow :: RandomGen g => Flow -> Flow -> g -> Flow
createHarmonyFlow (ch:rest) past gen
	| chance > rndChance = ch : createHarmonyFlow rest (ch:past) gen2
	| otherwise = createHarmonyFlow rest past gen2
	where
		chance = harmonyChance ch past
		(rndChance, gen2) = randomR (0.5 :: Float, 1) gen


createTimedFlow :: RandomGen g => Flow -> [Duration] -> Flow -> g -> Flow
createTimedFlow (har:harRest) (dur:durRest) past gen
	| isEnd endCh past && chanceEnd > rndChance = [endCh]
	| chance > rndChance = ch :
		createTimedFlow harRest durRest (ch:past2) gen2
	| otherwise = createTimedFlow (har:harRest) durRest past gen2
	where
		(pch:_) = past
		past2 = if (Types.dur pch == 0) then [] else past
		newRemain = let diff = remain pch - Types.dur pch in if diff > 0
			then diff else beat pch
		ch = TimedChord {
			tones = tones har, key = key har, intervals = intervals har,
			dur = dur, beat = beat pch, remain = newRemain}
		chance = rhythmChance ch past2
		endCh = TimedChord {
			tones = tones har, key = key har, intervals = intervals har,
			dur = newRemain, beat = beat pch, remain = newRemain}
		chanceEnd = rhythmChance endCh past2
		(rndChance, gen2) = randomR (0.5 :: Float, 1) gen


isEnd :: Chord -> Flow -> Bool
isEnd ch past = length past > 35 &&	isTonic tones1 key1 intervals1 &&
	2 * dur1 >= beat1
	where tones1 = tones ch; key1 = key ch; intervals1 = intervals ch;
		dur1 = dur ch; beat1 = beat ch

