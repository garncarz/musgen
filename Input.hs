module Input where

-- TODO ošetřit chyby vstupu

import List
import Maybe
import MGRandom
import Random
import Relations
import Types
import System.Environment

getStartChord :: IO Chord
getStartChord = do
	args <- getArgs
	gen <- newStdGen
	let
		g = rndSplitL gen
		arg name = if not (isNothing pos1) && pos2 < length args
			then args !! pos2 else "-"
			where pos1 = elemIndex ('-':name) args; pos2 = (fromJust pos1) + 1
		argValue name implicit = if arg name /= "-" then arg name else implicit
		
		key = read $ argValue "key"
			(let (i, _) = randomR (-6 :: Int, 6) (g !! 0) in show $ i + 76)
		scale = argValue "scale"
			(let (i, _) = randomR (0, 1) (g !! 1) in ["major", "minor"] !! i)
		measure = read $ argValue "measure" "16"
		beats = read $ argValue "beats" "4"
		
		startChord = Chord {tones = [], key = key,
			intervals = if scale == "minor" then minor else major,
			dur = 0, measure = measure, remain = measure, beats = beats}
	return startChord

