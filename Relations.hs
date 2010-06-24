module Relations where

import Types

scaleSize :: Int
scaleSize = 12

major :: Intervals
major = [0, 2, 4, 5, 7, 9, 11]

minor :: Intervals
minor = [0, 2, 3, 5, 7, 8, 10]


toneJump :: Tone -> [Tone] -> Bool
toneJump t pt = not isNear where
	tUp = t + 2
	tDown = t - 2
	isNear = elem tUp pt || elem tDown pt


intervalFrom :: Tone -> Tone -> Int
intervalFrom base tone = i where
	dist = (tone - base) `mod` scaleSize
	i = if dist < 0	then dist + scaleSize else dist

toneFromScale :: Tone -> Tone -> Intervals -> Bool
toneFromScale tone base intervals = elem (intervalFrom base tone) intervals

triadFrom :: Tone -> Intervals -> [Tone] -> Bool
triadFrom root i tones =
	foldl1 (&&) $
		map (\t -> elem (intervalFrom root t) [i !! 0, i !! 2, i !! 4]) tones

chordTriad :: [Tone] -> Intervals -> Bool
chordTriad tones i = foldl (||) False $ map (\t -> triadFrom t i tones) tones

