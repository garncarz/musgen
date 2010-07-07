module Main where

import Random

import ChanceHarmony
import ChanceRhythm
import InstrumentBass
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
		base = head $ rndTones (g !! 0)
		mjr = head $ randomRs (True, False) (g !! 1)
		st = MusicState {
			base = base,
			intervals = if mjr then major else minor,
			beat = 16,
			remain = 16}
		chordsSrc = rndChords (g !! 2)
		harmonyFlow = createHarmonyFlow chordsSrc [] st (g !! 3)
		dursSrc = rndDurations (g !! 4)
		flow = createTimedFlow harmonyFlow dursSrc [] st (g !! 5)
	print st
	mapM_ putStrLn $ map show flow
	--exportMidi "harmony.midi" $ midiFile [harmonyMidiFlow flow st]
	exportMidi "song.midi" $ midiFile [
		harmonyTrack flow st,
		sopranoTrack flow st,
		bassTrack flow st]


createHarmonyFlow :: RandomGen g => [Chord] -> [Chord] -> MusicState ->
	g -> [Chord]
createHarmonyFlow (ch:rest) past st gen
	| chance > rndChance = ch : createHarmonyFlow rest (ch:past) st gen2
	| otherwise = createHarmonyFlow rest past st gen2
	where
		chance = harmonyChance ch past st
		(rndChance, gen2) = randomR (0.5 :: Float, 1) gen


createTimedFlow :: RandomGen g => [Chord] -> [Duration] -> [TimedChord] ->
	MusicState -> g -> [TimedChord]
createTimedFlow (har:harRest) (dur:durRest) past st gen
	| isEnd endCh past endSt && chanceEnd > rndChance = [endCh]
	| chance > rndChance = ch :
		createTimedFlow harRest durRest (ch:past) newSt gen2
	| otherwise = createTimedFlow (har:harRest) durRest past st gen2
	where
		ch = (har, dur)
		chance = rhythmChance ch past st
		newSt = (cutMusSt st ch)
		endCh = (har, remain st)
		endSt = (cutMusSt st endCh)
		chanceEnd = rhythmChance endCh past st
		(rndChance, gen2) = randomR (0.5 :: Float, 1) gen
		

cutMusSt :: MusicState -> TimedChord -> MusicState
cutMusSt st (_, dur) = MusicState {
	base = base st,
	intervals = intervals st,
	beat = beat st,
	remain = newRemain}
	where
		diff = remain st - dur
		newRemain = if diff > 0 then diff else beat st

isEnd :: TimedChord -> Flow -> MusicState -> Bool
isEnd (tones, dur) past st = length past > 35 &&
	isTonic tones (base st) (intervals st) &&
	2 * dur >= beat st

