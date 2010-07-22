module Relations where

import List
import Maybe
import Types

scaleSize = 12 :: Int
major = [0, 2, 4, 5, 7, 9, 11] :: Intervals
minor = [0, 2, 3, 5, 7, 8, 10] :: Intervals


toneJumpFrom :: [Tone] -> Tone -> Interval
toneJumpFrom from tone = minimum $ map (\t -> abs $ t - tone) from

intervalFromTo :: Tone -> Tone -> Interval
intervalFromTo tone1 tone2 = i where
	dist = (tone2 - tone1) `mod` scaleSize
	i = if dist < 0 then dist + scaleSize else dist

intervalAt :: Intervals -> Int -> Tone
intervalAt intervals pos = intervals !! pos3 where
	pos2 = pos `mod` (length intervals)
	pos3 = if pos2 < 0 then pos2 + (length intervals) else pos2

isToneFromScale :: Tone -> Tone -> Intervals -> Bool
isToneFromScale tone key intervals = elem (intervalFromTo key tone) intervals

isTriadFrom :: Tone -> Tone -> Intervals -> [Tone] -> Bool
isTriadFrom _ _ _ [] = False
isTriadFrom root key intervals chord
	| pos == Nothing = False
	| otherwise = all (\t -> elem (intervalFromTo key t) [i1, i2, i3]) chord
	where
		pos = elemIndex (intervalFromTo key root) intervals
		pos1 = fromJust pos
		i1 = intervals !! pos1
		i2 = intervalAt intervals (pos1 + 2)
		i3 = intervalAt intervals (pos1 + 4)

isTriad :: [Tone] -> Tone -> Intervals -> Bool
isTriad chord key intervals =
	any (\t -> isTriadFrom t key intervals chord) chord

isFullTriad :: [Tone] -> Tone -> Intervals -> Bool
isFullTriad chord key intervals = isTriad chord key intervals &&
	length (nub $ map (\t -> intervalFromTo key t) chord) > 2

isTonic :: [Tone] -> Tone -> Intervals -> Bool
isTonic chord key intervals = isTriadFrom key key intervals chord

isSubdominant :: [Tone] -> Tone -> Intervals -> Bool
isSubdominant chord key intervals = isTriadFrom subRoot key intervals chord
	where subRoot = key + intervals !! 3
	
isDominant :: [Tone] -> Tone -> Intervals -> Bool
isDominant chord key intervals = isTriadFrom domRoot key intervals chord
	where domRoot = key + intervals !! 4

isMaxOneLeadingTone :: [Tone] -> Tone -> Intervals -> Bool
isMaxOneLeadingTone chord key intervals = length leadingTones <= 1 where
	leadingTones = nub $ filter (\t -> intervalFromTo key t == ltint) chord
	ltint = intervals !! (length intervals - 1)

isLeadingToneOk :: [Tone] -> [Tone] -> Tone -> Intervals -> Bool
isLeadingToneOk first second key intervals =
	let
		ltint = intervals !! (length intervals - 1)
		leadingTones tones = nub $ filter (\t -> intervalFromTo key t ==
			ltint) tones
		ltones1 = leadingTones first
		ltones2 = leadingTones second
		ltone = ltones1 !! 0
		tonic = ltone + scaleSize - ltint
	in
		length ltones2 < 2 && (ltones1 == [] || (ltones1 == [ltone] &&
			(elem ltone second || elem tonic second)))

isCounterpoint :: [Tone] -> [Tone] -> Bool
isCounterpoint first second = (maxUp && minDown) || (maxDown && minUp) where
	max1 = maximum first
	max2 = maximum second
	maxUp = max2 >= max1
	maxDown = max2 <= max1
	min1 = minimum first
	min2 = minimum second
	minUp = min2 >= min1
	minDown = min2 <= min1

isSopranoMoving :: [Tone] -> [Tone] -> Bool
isSopranoMoving first second = max1 /= max2 where
	max1 = maximum first
	max2 = maximum second

isBassMoving :: [Tone] -> [Tone] -> Bool
isBassMoving first second = min1 /= min2 where
	min1 = minimum first
	min2 = minimum second

percentageMoving :: [Tone] -> [Tone] -> Float
percentageMoving first second = moves / genericLength second where
	moves = sum $ map (\t -> if elem t first then 0 else 1 :: Float) second

