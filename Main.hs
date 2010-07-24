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


main = main2


main1 :: IO()
main1 = do
	gen <- newStdGen
	let
		g = rndSplitL gen
		--key = head $ rndTones (g !! 0)
		key = let (i, _) = randomR (-6, 6) (g !! 0) in i + 64
		(mjr, _) = randomR (True, False) (g !! 1)
		startHarmony = HarmonyChord {tones = [], key = key,
			intervals = if mjr then major else minor}
		chordsSrc = rndChords startHarmony (g !! 2)
		harmonyFlow = createHarmonyFlow chordsSrc [] (g !! 3)
		startRhythm = TimedChord {tones = [], key = 0, intervals = [],
			dur = 0, measure = 16, remain = 16, beats = 4}
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
			then diff else measure pch
		ch = pch {
			tones = tones har, key = key har, intervals = intervals har,
			dur = dur, remain = newRemain}
		chance = rhythmChance ch past2
		endCh = ch {dur = newRemain}
		--chanceEnd = rhythmChance endCh past2
		chanceEnd = 1
		(rndChance, gen2) = randomR (0.5 :: Float, 1) gen

isEnd :: Chord -> Flow -> Bool
isEnd ch past = length past > 40 && isTonicTriadIn key1 intervals1 tones1 &&
	2 * dur1 >= measure1
	where tones1 = tones ch; key1 = key ch; intervals1 = intervals ch;
		dur1 = dur ch; measure1 = measure ch



main2 :: IO()
main2 = do
	gen <- newStdGen
	let
		g = rndSplitL gen
		--key = head $ rndTones (g !! 0)
		key = let (i, _) = randomR (-6, 6) (g !! 0) in i + 64
		(mjr, _) = randomR (True, False) (g !! 1)
		startHarmony = HarmonyChord {tones = [], key = key,
			intervals = if mjr then major else minor}
		harFlow = harmonyFlow [startHarmony] (g !! 2)
		startRhythm = TimedChord {tones = [], key = 0, intervals = [],
			dur = 0, measure = 16, remain = 16, beats = 4}
		flow = rhythmicFlow harFlow [startRhythm] (g !! 3)
	mapM_ putStrLn $ map show flow
	exportMidi "song.midi" $ midiFile [harmonyTrack flow, sopranoTrack flow,
		bassTrack flow]


harmonyFlow :: RandomGen g => Flow -> g -> Flow
harmonyFlow past gen = ch : harmonyFlow (ch : (realHPast past)) g1
	where (g1, g2) = split gen; ch = nextInHarmony past g2

nextInHarmony :: RandomGen g => Flow -> g -> Chord
nextInHarmony past gen = if canBeNextInHarmony ch past (g !! 0)
	then ch	else nextInHarmony past (g !! 1)
	where g = rndSplitL gen; ch = head $ rndChords state (g !! 2);
		(state:_) = past

canBeNextInHarmony :: RandomGen g => Chord -> Flow -> g -> Bool
canBeNextInHarmony ch past gen = chance > rndChance
	where chance = harmonyChance ch (realHPast past);
		(rndChance, _) = randomR (0.5 :: Float, 1) gen

realHPast :: Flow -> Flow
realHPast past = if (tones pch == [] && length past == 1) then [] else past
	where (pch:_) = past


rhythmicFlow :: RandomGen g => Flow -> Flow -> g -> Flow
rhythmicFlow harFlow past gen = if isEnd then [endCh]
	else ch : rhythmicFlow harRest (ch : (realRPast past)) g1
	where
		(har:harRest) = harFlow
		(g1, g2) = split gen
		ch = nextInRhythm past har g2
		endCh = createEndChord ch
		isEnd = canBeEnd endCh past &&
			rhythmChance endCh (realRPast past) > floatMin

nextInRhythm :: RandomGen g => Flow -> Chord -> g -> Chord
nextInRhythm past har gen = if canBeNextInRhythm ch past (g !! 0)
	then ch	else nextInRhythm past har (g !! 1)
	where
		g = rndSplitL gen
		dur = head $ rndDurations (g !! 2)
		(pch:_) = past
		newRemain = let diff = remain pch - Types.dur pch in if diff > 0
			then diff else measure pch
		ch = pch {
			tones = tones har, key = key har, intervals = intervals har,
			dur = dur, remain = newRemain}
		(rndChance, _) = randomR (0.5 :: Float, 1) (g !! 3)

canBeNextInRhythm :: RandomGen g => Chord -> Flow -> g -> Bool
canBeNextInRhythm ch past gen = chance > rndChance
	where chance = rhythmChance ch (realRPast past);
		(rndChance, _) = randomR (0.5 :: Float, 1) gen

realRPast :: Flow -> Flow
realRPast past = if (dur pch == 0) then [] else past
	where (pch:_) = past


createEndChord :: Chord -> Chord
createEndChord ch = ch {dur = remain ch}

canBeEnd :: Chord -> Flow -> Bool
canBeEnd ch past = length past > 40 && isTonicTriadIn key1 intervals1 tones1 &&
	2 * dur1 >= measure1
	where tones1 = tones ch; key1 = key ch; intervals1 = intervals ch;
		dur1 = dur ch; measure1 = measure ch

