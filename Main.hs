module Main where

import Random

import ChanceHarmony
import ChanceRhythm
import Midi
import MGRandom
import Relations
import Types


cMaj :: TimedChord
cMaj = ([60, 64, 67], 16)

startMS = MusicState {
	base = 65,
	intervals = major,
	beat = 16,
	remain = 16
	}


main :: IO()
main = do
	let st = startMS
	gen <- newStdGen
	let g = rndSplitL gen
	let chordsSrc = rndChords (g !! 0)
	let harmonyFlow = createHarmonyFlow chordsSrc [] st
	let dursSrc = rndDurations (g !! 1)
	let flow = createTimedFlow harmonyFlow dursSrc [] st
	mapM_ putStrLn $ map show flow
	exportFlow flow st


createHarmonyFlow :: [Chord] -> [Chord] -> MusicState -> [Chord]
createHarmonyFlow (ch:rest) past st
	| chance > 0.5 = ch : createHarmonyFlow rest (ch:past) st
	| otherwise = createHarmonyFlow rest past st
	where chance = harmonyChance ch past st


createTimedFlow :: [Chord] -> [Duration] -> [TimedChord] -> MusicState ->
	[TimedChord]
createTimedFlow (har:harRest) (dur:durRest) past st
	| isEnd endCh past endSt && chanceEnd > 0.5 = [endCh]
	| chance > 0.5 = ch : createTimedFlow harRest durRest (ch:past) newSt
	| otherwise = createTimedFlow (har:harRest) durRest past st
	where
		ch = (har, dur)
		chance = rhythmChance ch past st
		newSt = (cutMusSt st ch)
		endCh = (har, remain st)
		endSt = (cutMusSt st endCh)
		chanceEnd = rhythmChance endCh past st
		

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
	isTonic tones (base st) (intervals st)

