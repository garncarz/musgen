module Main where

import Random

import Midi
import MGRandom
import Chances
import Relations
import Types


cMaj :: Chord
cMaj = ([60, 64, 67], 16)


main :: IO()
main = do
	gen <- newStdGen
	let chordsSrc = rndChords gen
	let chords = createFlow chordsSrc [] (62, minor)
	print chords
	exportFlow chords


createFlow :: Flow -> Flow -> MusicState -> Flow
createFlow (ch:rest) past st
	| chance ch past st > 0.5 && isEnd ch past st = [ch]
	| chance ch past st > 0.5 = ch : createFlow rest (ch:past) st
	| otherwise = createFlow rest past st

isEnd :: Chord -> Flow -> MusicState -> Bool
isEnd (tones, dur) past (base, intervals) = length past > 35 &&
	isTonic tones base intervals

