module MGRandom (rndSplitL, rndTones, rndChordTones, rndDuration) where

import List
import Random
import Types

rndDuration :: RandomGen g => g -> Duration
rndDuration gen = max 2 $ 2 * rnd where (rnd, _) = randomR (0 :: Int, 8) gen

rndTonesCount :: RandomGen g => g -> Int
rndTonesCount gen = 4

rndTones :: RandomGen g => g -> [Tone]
rndTones gen =
	let	(g1, g2) = split gen
	in rndNormal 0 127 g1 : rndTones g2
	--in (head $ randomRs (0, 127) g1) : rndTones g2

rndChordTones :: RandomGen g => g -> [Tone]
rndChordTones = sort . rndChordTones2
rndChordTones1 gen = take count tones where
	count = rndTonesCount g1; tones = rndTones g2; (g1, g2) = split gen
rndChordTones2 gen = take count $ nub [arr i | i <- [1..2 * count]] where
	(g1, g2) = split gen
	intervals = rndIntervals g1; count = rndTonesCount g2
	arr 0 = 64 + intervals !! 0; arr i = arr (i - 1) + intervals !! i

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


testRndNormal :: IO ()
testRndNormal = do
	gen <- newStdGen
	let tones = take 10000 $ rndTones gen
	let occur tone tones
		| tone == 128 = []
		| otherwise = (tone, (length $ findIndices (== tone) tones)) :
			occur (tone + 1) tones
	mapM_ putStrLn $ map (\(x, y) -> show x ++ "\t" ++ show y) $ occur 0 tones

