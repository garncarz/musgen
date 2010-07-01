module Main where

import Random

import Midi
import MGRandom
import Chances
import Relations
import Types


cMaj :: Chord
cMaj = ([60, 64, 67], 16)

startMS = MusicState {
	base = 60,
	intervals = major,
	beat = 16,
	remain = 16
	}


main :: IO()
main = do
	gen <- newStdGen
	let chordsSrc = rndChords gen
	let chords = createFlow chordsSrc [] startMS (nullChord, 0)
	mapM_ putStrLn $ map show chords
	exportFlow chords


cutMusSt :: MusicState -> Chord -> MusicState
cutMusSt st (_, dur) = MusicState {
	base = base st,
	intervals = intervals st,
	beat = beat st,
	remain = newRemain}
	where
		diff = remain st - dur
		newRemain = if diff > 0 then diff else beat st

createFlow :: Flow -> Flow -> MusicState -> (Chord, Int) -> Flow
createFlow (ch:rest) past st (bestSoFar, bestFor)
	| chanceCh > floatMin && isEnd ch past st = [ch]
	| chanceCh > 0.5 = ch : createFlow rest (ch:past) (cutMusSt st ch) nullBest
	| bestFor > 30 && chanceBest > floatMin = bestSoFar :
		createFlow rest (bestSoFar:past) (cutMusSt st bestSoFar) nullBest
	| chanceCh > chanceBest =
		createFlow rest past st (ch, bestFor + 1)
	| otherwise = createFlow rest past st (bestSoFar, bestFor + 1)
	where
		chanceCh = chance ch past st
		chanceBest = chance bestSoFar past st
		nullBest = (ch, 0)

isEnd :: Chord -> Flow -> MusicState -> Bool
isEnd (tones, dur) past st = length past > 35 &&
	isTonic tones (base st) (intervals st)

