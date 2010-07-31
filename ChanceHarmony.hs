module ChanceHarmony (harmonyChance) where

import List
import Types
import Relations

-- TODO přidat pravidlo proti opakování stejných [Tone]

harmonyChance :: ChanceType
harmonyChance now past = foldl (*) 1
	(map (\(chance, factor) -> (** factor) $ chance now past) chances)

chances :: [(ChanceType, Float)]
chances = [
	(chanceLeadingTone, 1),
	(chanceTonicStart, 1),
	(chanceNotDomThenSub, 1),
	(chanceInScale, 0.4),
	(chanceThick, 0.8),
	(chanceJumps, 0.6),
	(chanceTriad, 0.9),
	(chanceNotEmpty, 0.9),
	--(chance4Tones, 0.5),
	(chanceCounterpoint, 1),
	(chanceMove, 1),
	(chanceConsonance, 0.8)
	]

chanceThick now _ = if rng > 30 || dist > 10 then floatMin else 1
	where
		tones1 = tones now; key1 = key now
		max = maximum tones1; min = minimum tones1
		rng = max - min
		avg = sum tones1 `div` length tones1
		dist = abs $ avg - key1

chanceJumps _ [] = 1
chanceJumps now (past:_) = if avg < 5 && maxJump < 5 then 1 else floatMin
	where
		tones1 = tones past; tones2 = tones now
		jumps = map (toneJumpFrom tones1) tones2
		avg = fromIntegral (sum jumps) / genericLength tones2
		maxJump = maximum jumps

chanceInScale now _ =
	if areFromScale key1 intervals1 tones1 then 1 else floatMin
	where tones1 = tones now; key1 = key now; intervals1 = intervals now

chanceTriad now _ = if null tones1 || isFullTriad tones1 then 1 else floatMin
	where tones1 = tones now

chanceTonicStart now [] =
	if isTonicTriadIn key1 intervals1 tones1 then 1 else 0
	where tones1 = tones now; key1 = key now; intervals1 = intervals now
chanceTonicStart _ _ = 1

chanceNotDomThenSub now (past:_) = if isDominantIn key1 intervals1 tones1 &&
	isSubdominantIn key2 intervals2 tones2 then floatZero else 1
	where tones1 = tones past; key1 = key past; intervals1 = intervals past;
		tones2 = tones now; key2 = key now; intervals2 = intervals now
chanceNotDomThenSub _ [] = 1

chanceLeadingTone now (past:_) =
	if isLeadingToneOkIn key2 intervals2 tones1 tones2 then 1 else 0
	where tones1 = tones past; tones2 = tones now; key2 = key now;
		intervals2 = intervals now
chanceLeadingTone now [] =
	if isLeadingToneOkIn key1 intervals1 [] tones1 then 1 else 0
	where tones1 = tones now; key1 = key now; intervals1 = intervals now

chanceNotEmpty now _ = if tones now == [] then floatMin else 1

chance4Tones now _ = if length (nub $ tones now) == 4 then 1 else floatMin

chanceCounterpoint now (past:_) =
	if isCounterpoint tones1 tones2 then 1 else floatMin
	where tones1 = tones past; tones2 = tones now
chanceCounterpoint _ [] = 1

chanceMove now (past:_)
	| not sopMv = floatMin
	| not bassMv = floatHalf
	| pct > 0.5 = 1
	| otherwise = 0.8
	where
		tones1 = tones past; tones2 = tones now
		sopMv = isSopranoMoving tones1 tones2
		bassMv = isBassMoving tones1 tones2
		pct = percentageMoving tones1 tones2
chanceMove _ [] = 1

chanceConsonance now (past:_) = if isConsonantIn key2 intervals2 tones2
	|| isConsonantIn key1 intervals1 tones1 then 1 else floatMin
	where
		tones1 = tones past; key1 = key past; intervals1 = intervals past
		tones2 = tones now; key2 = key now; intervals2 = intervals now
chanceConsonance now [] =
	if isConsonantIn key1 intervals1 tones1 then 1 else floatMin
	where tones1 = tones now; key1 = key now; intervals1 = intervals now

