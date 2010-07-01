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
isTriadFrom _ _ _ [] = False
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

isTriad :: [Tone] -> Tone -> Intervals -> Bool
isTriad tones base i = foldl (||) False $
	map (\t -> isTriadFrom t base i tones) tones

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

