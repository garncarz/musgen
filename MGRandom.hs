module MGRandom (rndChords, rndDurations, rndSplitL, rndTones) where

import List

import Random
import Types

rndTonesCount :: RandomGen g => g -> Int
rndTonesCount gen = 4

rndDurations :: RandomGen g => g -> [Duration]
rndDurations gen = let (exp, gen2) = randomR (0 :: Int, 5) gen
	in 2 ^ exp : rndDurations gen2
--rndDuration gen = let (dur, _) = randomR (1, 32) gen in dur

rndTones :: RandomGen g => g -> [Tone]
rndTones gen =
	let	(g1, g2) = split gen
	in rndNormal 0 127 g1 : rndTones g2

rndChords :: RandomGen g => g -> [Chord]
rndChords gen =
	let
		g = rndSplitL gen
		tonesCount = rndTonesCount (g !! 0)
		tones = take tonesCount $ rndTones (g !! 1)
	in tones : rndChords (g !! 2)

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


testRndNormal :: IO()
testRndNormal = do
	gen <- newStdGen
	let tones = take 10000 $ rndTones gen
	let occur tone tones
		| tone == 128 = []
		| otherwise = (tone, (length $ findIndices (== tone) tones)) :
			occur (tone + 1) tones
	mapM_ putStrLn $ map (\(x, y) -> show x ++ "\t" ++ show y) $ occur 0 tones

