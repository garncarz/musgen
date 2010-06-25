module MusGen where

import Random

import Midi
import Chances
import Relations
import Types


cMaj :: Chord
cMaj = ([60, 64, 67], 16)


main :: IO()
main = do
	gen <- newStdGen
	let chordsSrc = rndChords gen
	let chords = createFlow chordsSrc [] (60, minor)
	exportFlow chords
	print chords


rndChords :: RandomGen g => g -> Flow
rndChords g =
	let
		(tonesCount, g2) = randomR (0, 5) g
		(dur, g3) = randomR (1, 30) g2
		(g4, g5) = split g3
		tones = take tonesCount $ randomRs (0, 127) g4
	in (tones, dur) : rndChords g5


createFlow :: Flow -> Flow -> MusicState -> Flow
createFlow (ch:rest) past st
	| chance ch past st > 0.5 && isEnd ch past st = [ch]
	| chance ch past st > 0.5 = ch : createFlow rest (ch:past) st
	| otherwise = createFlow rest past st

isEnd :: Chord -> Flow -> MusicState -> Bool
isEnd (tones, dur) past st = length past > 20

