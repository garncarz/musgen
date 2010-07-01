module MGRandom where

import List

import Random
import Types

rndTonesCount :: RandomGen g => g -> Int
rndTonesCount gen = 4

rndDuration :: RandomGen g => g -> Duration
rndDuration gen = let (exp, _) = randomR (0 :: Int, 5) gen in 2 ^ exp
--rndDuration gen = let (dur, _) = randomR (1, 32) gen in dur

rndTones :: RandomGen g => g -> [Tone]
rndTones gen =
	let	(g1, g2) = split gen
	in rndNormal 0 127 g1 : rndTones g2

rndChords :: RandomGen g => g -> Flow
rndChords gen =
	let
		g = rndSplitL gen
		tonesCount = rndTonesCount (g !! 0)
		dur = rndDuration (g !! 1)
		tones = take tonesCount $ rndTones (g !! 2)
	in (tones, dur) : rndChords (g !! 3)

rndNormal :: RandomGen g => Int -> Int -> g -> Int
rndNormal from to gen = maximum [from, minimum [to, result]] where
	size = to - from
	center = fromIntegral from + fromIntegral size / 2
	rnds = randomRs (0 :: Float, 1) gen
	number = sum $ take size rnds
	--result = truncate $ 5 * (number - center) + center
	result = truncate number

rndSplitL :: RandomGen g => g -> [g]
rndSplitL g = let (g1, g2) = split g in g1 : rndSplitL g2


testRndNormal = do
	gen <- newStdGen
	let tones = take 10000 $ rndTones gen
	let occur tone tones
		| tone == 128 = []
		| otherwise = (tone, (length $ findIndices (== tone) tones)) :
			occur (tone + 1) tones
	mapM_ putStrLn $ map (\(x, y) -> show x ++ "\t" ++ show y) $ occur 0 tones

