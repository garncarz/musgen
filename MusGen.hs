module MusGen where

import Random

import Midi
import Penalties
import Types


cMaj :: Chord
cMaj = ([60, 64, 67], 16)


main :: IO()
main = do {
	newStdGen;
	gen <- getStdGen;
	let chordsSrc = rndChords gen
	; let chords = createFlow chordsSrc
	; exportFlow chords;
	print chords
}


rndChords :: RandomGen g => g -> [Chord]
rndChords g =
	let
		(tonesCount, g2) = randomR (1, 5) g
		(dur, g3) = randomR (1, 30) g2
		(g4, g5) = split g3
		tones = take tonesCount $ randomRs (0, 127) g4
	in (tones, dur) : rndChords g5


createFlow :: [Chord] -> [Chord]
createFlow (ch:rest)
	| isEnd ch = ch : []
	| penalty ch <= 0.5 = ch : createFlow rest
	| otherwise = createFlow rest

isEnd :: Chord -> Bool
isEnd (tones, dur) = tones == [60]

