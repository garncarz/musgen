module Flow where

import ChanceHarmony
import ChanceHarmonyRhythm
import MGRandom
import Random
import Relations
import Types

loadFlow :: String -> IO Flow
loadFlow filename = do
	content <- readFile filename
	let flow = map read $ lines content
	putStrLn "Loading flow..."
	return flow

produceFlow :: Chord -> Int -> String -> IO Flow
produceFlow startChord minMeasures filename = do
	gen <- newStdGen
	let flow = nextFlow [startChord] 0 minMeasures gen
	mapM_ (putStrLn . showBrief) flow
	writeFile filename $ unlines $ map show flow
	putStrLn "Flow generated."
	return flow

nextFlow :: RandomGen g => Flow -> Int -> Int -> g -> Flow
nextFlow past measures minMeasures gen = if isEnd then [endCh]
	else ch : nextFlow (ch:rpast) measures2 minMeasures (g !! 2)
	where
		ch1 = nextTonesChord past (g !! 0)
		endCh = ch1 {dur = remain ch1}
		isEnd = measures >= minMeasures && canBeEnd endCh rpast
		ch = nextDurChord ch1 past (g !! 1)
		measures2 = if measure ch == remain ch then measures + 1 else measures
		rpast = realPast past; (pch:_) = past
		g = rndSplitL gen

nextTonesChord :: RandomGen g => Flow -> g -> Chord
nextTonesChord past gen = if ok then ch else nextTonesChord past (g !! 2) where
	ok = chance >= minChance; chance = harmonyChance ch rpast
	ch = pch {tones = rndChordTones (g !! 0), dur = 0, remain = newRemain}
	newRemain = let diff = remain pch - Types.dur pch in if diff > 0
			then diff else measure pch
	rpast = realPast past; (pch:_) = past
	g = rndSplitL gen
	(minChance, _) = randomR (0.5 :: Float, 0.7) (g !! 1)

nextDurChord :: RandomGen g => Chord -> Flow -> g -> Chord
nextDurChord ch past gen = if ok then ch1 else nextDurChord ch past (g !! 2)
	where
		ok = chance >= minChance; chance = harmonyRhythmChance ch1 rpast
		ch1 = ch {dur = rndDuration (g !! 0)}
		rpast = realPast past
		g = rndSplitL gen
		--(minChance, _) = randomR (0.5 :: Float, 0.7) (g !! 1)
		minChance = 0.5

canBeEnd :: Chord -> Flow -> Bool
canBeEnd ch past = isTonicTriadIn key1 intervals1 tones1 && 2 * dur1 >= measure1
	where tones1 = tones ch; key1 = key ch; intervals1 = intervals ch;
		dur1 = dur ch; measure1 = measure ch

realPast :: Flow -> Flow
realPast past = if dur pch == 0 && tones pch == [] then [] else past
	where (pch:_) = past

