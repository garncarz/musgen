module Relations where

import List
import Maybe
import Types

scaleSize :: Int
scaleSize = 12

major :: Intervals
major = [0, 2, 4, 5, 7, 9, 11]

minor :: Intervals
minor = [0, 2, 3, 5, 7, 8, 10]


isToneJump :: Tone -> [Tone] -> Bool
isToneJump t pt = not isNear where
	tUp = t + 2
	tDown = t - 2
	isNear = elem tUp pt || elem tDown pt

intervalFromTo :: Tone -> Tone -> Int
intervalFromTo base tone = i where
	dist = (tone - base) `mod` scaleSize
	i = if dist < 0 then dist + scaleSize else dist

intervalAt :: Intervals -> Int -> Tone
intervalAt intervals pos = posN where
	pos2 = pos `mod` (length intervals)
	posN = if pos2 < 0 then pos2 + (length intervals) else pos2

isToneFromScale :: Tone -> Tone -> Intervals -> Bool
isToneFromScale tone base intervals = elem (intervalFromTo base tone) intervals

isTriadFrom :: Tone -> Tone -> Intervals -> [Tone] -> Bool
isTriadFrom root base i tones
	| pos == Nothing = False
	| otherwise = foldl1 (&&) $
		map (\t -> elem (intervalFromTo base t)
			[i !! pos1, i !! pos2, i !! pos3]) tones
	where
		pos = elemIndex (intervalFromTo base root) i
		pos1 = fromJust pos
		pos2 = intervalAt i (pos1 + 2)
		pos3 = intervalAt i (pos1 + 4)

isChordTriad :: [Tone] -> Tone -> Intervals -> Bool
isChordTriad tones base i = foldl (||) False $
	map (\t -> isTriadFrom t base i tones) tones

