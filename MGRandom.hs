module MGRandom (rndChords, rndDurations, rndSplitL, rndTones) where

import List
import Random
import Types

rndTonesCount :: RandomGen g => g -> Int
rndTonesCount gen = 4

rndDurations :: RandomGen g => g -> [Duration]
rndDurations gen = let (rnd, gen2) = randomR (1 :: Int, 8) gen
	in 2 * rnd : rndDurations gen2
--rndDurations gen = 4 : rndDurations gen

rndTones :: RandomGen g => g -> [Tone]
rndTones gen =
	let	(g1, g2) = split gen
	in rndNormal 0 127 g1 : rndTones g2
	--in (head $ randomRs (0, 127) g1) : rndTones g2

rndChords :: RandomGen g => Chord -> g -> Flow
rndChords = rndChords2

rndChords1 :: RandomGen g => Chord -> g -> Flow
rndChords1 start gen =
	let
		g = rndSplitL gen
		tonesCount = rndTonesCount (g !! 0)
		tones = take tonesCount $ rndTones (g !! 1)
	in start {tones = tones} : rndChords1 start (g !! 2)

rndChords2 :: RandomGen g => Chord -> g -> Flow
rndChords2 state gen = ch : rndChords2 state (g !! 0) where
	g = rndSplitL gen
	intervals = rndIntervals (g !! 1)
	first = key state + intervals !! 0
	second = first + intervals !! 1
	third = second + intervals !! 2
	fourth = third + intervals !! 3
	tones = [first, second, third, fourth]
	ch = state {tones = tones}

rndIntervals :: RandomGen g => g -> Intervals
rndIntervals gen = int : rndIntervals gOut where
	(nr, gS) = randomR (0, 100 :: Int) gen
	(sign, g2) = randomR (True, False) gS
	(gOut, gT) = split g2
	int1 = if nr < 25 then 0
		else if nr < 40 then 4
		else if nr < 55 then 3
		else if nr < 65 then 2
		else if nr < 75 then 5
		else if nr < 80 then 1
		else if nr < 90 then 6
		else if nr < 95 then 7
		else let (t, _) = randomR (0, 12) gT in t
	int = if sign then int1 else negate int1

rndNormal :: RandomGen g => Int -> Int -> g -> Int
rndNormal from to gen = maximum [from, minimum [to, result]] where
	size = to - from
	center = fromIntegral from + fromIntegral size / 2
	rnds = randomRs (0 :: Float, 1) gen
	number = sum $ take size rnds
	result = truncate $ 2 * (number - center) + center
	--result = truncate number

rndSplitL :: RandomGen g => g -> [g]
rndSplitL g = let (g1, g2) = split g in g1 : rndSplitL g2


testRndNormal :: IO()
testRndNormal = do
	gen <- newStdGen
	let tones = take 10000 $ rndTones gen
	let occur tone tones
		| tone == 128 = []
		| otherwise = (tone, (length $ findIndices (== tone) tones)) :
			occur (tone + 1) tones
	mapM_ putStrLn $ map (\(x, y) -> show x ++ "\t" ++ show y) $ occur 0 tones

