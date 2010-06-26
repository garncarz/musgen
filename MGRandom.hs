module MGRandom where

import List

import Random
import Types

rndTonesCount :: RandomGen g => g -> Int
rndTonesCount gen = rndGauss 0 10 gen

rndDuration :: RandomGen g => g -> Duration
rndDuration gen = rndGauss 1 30 gen

rndTones :: RandomGen g => g -> [Tone]
rndTones gen =
	let	(g1, g2) = split gen
	in rndGauss 0 127 g1 : rndTones g2

rndChords :: RandomGen g => g -> Flow
rndChords gen =
	let
		g = rndSplitL gen
		tonesCount = rndTonesCount (g !! 0)
		dur = rndDuration (g !! 1)
		tones = take tonesCount $ rndTones (g !! 2)
	in (tones, dur) : rndChords (g !! 3)


rndGaussDist :: RandomGen g => Int -> g -> Int
rndGaussDist max gen =
	let	(dist, _) = randomR (0 :: Float, 1 :: Float) gen
	in truncate $ fromIntegral max * (1 - sqrt dist)

rndGauss :: RandomGen g => Int -> Int -> g -> Int
rndGauss from to gen
	| from > to = error "rndGauss: from > to"
	| from == to = from
	| otherwise =
		let
			(sign, gen2) = randomR (True, False) gen
			minus = if sign then (-1) else 1
			dist = (to - from) `div` 2
			center = from + dist
			rndDist = rndGaussDist dist gen2
		in center + minus * rndDist


rndSplitL :: RandomGen g => g -> [g]
rndSplitL g = let (g1, g2) = split g in g1 : rndSplitL g2


rndTestGauss = do
	gen <- newStdGen
	let
		tones = take 10000 $ rndTones gen
		occur tone tones
			| tone == 128 = []
			| otherwise = (tone, (length $ findIndices (== tone) tones)) :
				occur (tone + 1) tones
	mapM_ putStrLn $ map (\(x, y) -> show x ++ " - " ++ show y) $ occur 0 tones

