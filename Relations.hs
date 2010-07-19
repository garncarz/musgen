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
isTriadFrom _ _ _ [] = False
isTriadFrom root base i tones
	| pos == Nothing = False
	| otherwise = all (\t -> elem (intervalFromTo base t)
		[i !! pos1, i !! pos2, i !! pos3]) tones
	where
		pos = elemIndex (intervalFromTo base root) i
		pos1 = fromJust pos
		pos2 = intervalAt i (pos1 + 2)
		pos3 = intervalAt i (pos1 + 4)

isTriad :: [Tone] -> Tone -> Intervals -> Bool
isTriad tones base i = any (\t -> isTriadFrom t base i tones) tones

isFullTriad :: [Tone] -> Tone -> Intervals -> Bool
isFullTriad tones base i = isTriad tones base i &&
	length (nub $ map (\t -> intervalFromTo base t) tones) > 2

isTonic :: [Tone] -> Tone -> Intervals -> Bool
isTonic ch base intervals = isTriadFrom base base intervals ch

isSubdominant :: [Tone] -> Tone -> Intervals -> Bool
isSubdominant ch base intervals = isTriadFrom subRoot base intervals ch where
	subRoot = base + intervals !! 3
	
isDominant :: [Tone] -> Tone -> Intervals -> Bool
isDominant ch base intervals = isTriadFrom domRoot base intervals ch where
	domRoot = base + intervals !! 4

isMaxOneLeadingTone :: [Tone] -> Tone -> Intervals -> Bool
isMaxOneLeadingTone ch base intervals = length leadingTones <= 1 where
	leadingTones = nub $ filter (\t -> intervalFromTo base t ==
			intervals !! (length intervals - 1)) ch

isLeadingToneOk :: [Tone] -> [Tone] -> Tone -> Intervals -> Bool
isLeadingToneOk first second base intervals =
	let
		ltint = intervals !! (length intervals - 1)
		leadingTones tones = nub $ filter (\t -> intervalFromTo base t ==
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
percentageMoving first second = fromIntegral moves / genericLength second where
	moves = sum $ map (\t -> if elem t first then 1 else 0) second

