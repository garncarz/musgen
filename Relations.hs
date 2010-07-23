module Relations where

import List
import Maybe
import Types

scaleSize = 12 :: Int
major = [0, 2, 4, 5, 7, 9, 11] :: Intervals
minor = [0, 2, 3, 5, 7, 8, 10] :: Intervals
chordIntervals = [
	majorTriad, minorTriad,
	diminishedTriad, augmentedTriad] :: [Intervals]
majorTriad = [0, 4, 7]
minorTriad = [0, 3, 7]
diminishedTriad = [0, 3, 6]
augmentedTriad = [0, 4, 8]


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

isFromScale :: Tone -> Intervals -> Tone -> Bool
isFromScale key intervals tone = elem (intervalFromTo key tone) intervals

areFromScale :: Tone -> Intervals -> [Tone] -> Bool
areFromScale key intervals tones = all (isFromScale key intervals) tones

fitsIntervalsFrom :: Intervals -> Tone -> [Tone] -> Bool
fitsIntervalsFrom _ _ [] = False
fitsIntervalsFrom intervals root chord =
	all (\t -> elem (intervalFromTo root t) intervals) chord

fitsIntervals :: Intervals -> [Tone] -> Bool
fitsIntervals intervals chord =
	any (\t -> fitsIntervalsFrom intervals t chord) chord

hasRoot :: Tone -> [Tone] -> Bool
hasRoot root chord =
	any (\i -> fitsIntervalsFrom i root chord) chordIntervals

isTriad :: [Tone] -> Bool
isTriad chord = any (\i -> fitsIntervals i chord)
	[majorTriad, minorTriad, diminishedTriad, augmentedTriad]

isFullTriad :: [Tone] -> Bool
isFullTriad chord = isTriad chord && length chIntervals > 2
	where
		first = chord !! 0
		chIntervals = nub $ map (intervalFromTo first) chord

isTonicTriadIn :: Tone -> Intervals -> [Tone] -> Bool
isTonicTriadIn key intervals chord = isTriad chord && hasRoot key chord

isSubdominantIn :: Tone -> Intervals -> [Tone] -> Bool
isSubdominantIn key intervals chord = hasRoot subRoot chord
	where subRoot = key + intervals !! 3

isDominantIn :: Tone -> Intervals -> [Tone] -> Bool
isDominantIn key intervals chord = hasRoot domRoot chord
	where domRoot = key + intervals !! 4

isLeadingToneOkIn :: Tone -> Intervals -> [Tone] -> [Tone] -> Bool
isLeadingToneOkIn key intervals first second =
	let
		ltint = intervals !! (length intervals - 1)
		leadingTones tones = nub $ filter (\t -> intervalFromTo key t ==
			ltint) tones
		ltones1 = leadingTones first; ltones2 = leadingTones second
		ltone = ltones1 !! 0; tonic = ltone + scaleSize - ltint
	in
		length ltones2 < 2 && (ltones1 == [] || (ltones1 == [ltone] &&
			(elem ltone second || elem tonic second)))

isCounterpoint :: [Tone] -> [Tone] -> Bool
isCounterpoint first second = (maxUp && minDown) || (maxDown && minUp) where
	max1 = maximum first; max2 = maximum second
	maxUp = max2 >= max1; maxDown = max2 <= max1
	min1 = minimum first; min2 = minimum second
	minUp = min2 >= min1; minDown = min2 <= min1

isSopranoMoving :: [Tone] -> [Tone] -> Bool
isSopranoMoving first second = max1 /= max2
	where max1 = maximum first; max2 = maximum second

isBassMoving :: [Tone] -> [Tone] -> Bool
isBassMoving first second = min1 /= min2
	where min1 = minimum first; min2 = minimum second

percentageMoving :: [Tone] -> [Tone] -> Float
percentageMoving first second = moves / genericLength second where
	moves = sum $ map (\t -> if elem t first then 0 else 1 :: Float) second

isConsonantIn :: Tone -> Intervals -> [Tone] -> Bool
isConsonantIn key intervals chord = areFromScale key intervals chord

