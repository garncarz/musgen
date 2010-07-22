module ChanceHarmony (harmonyChance) where

import List
import Types
import Relations

harmonyChance :: ChanceType
harmonyChance ch past = foldl (*) 1
	(map (\(chance, factor) -> (** factor) $ chance ch past) chances)

chances :: [(ChanceType, Float)]
chances = [
	(chanceLeadingTone, 1),
	(chanceTonicStart, 1),
	(chanceNotDomThenSub, 1),
	--(chanceInScale, 0.4),
	(chanceThick, 0.8),
	(chanceJumps, 0.6),
	(chanceTriad, 0.9),
	(chanceNotEmpty, 0.9),
	--(chance4Tones, 0.5),
	(chanceCounterpoint, 1),
	(chanceMove, 1)
	]

chanceThick ch _ = if rng > 30 || dist > 10 then floatMin else 1
	where
		tones1 = tones ch; key1 = key ch
		max = maximum tones1; min = minimum tones1
		rng = max - min
		avg = sum tones1 `div` length tones1
		dist = abs $ avg - key1

chanceJumps _ [] = 1
chanceJumps ch (past:_) = if avg < 5 && maxJump < 5 then 1 else floatMin
	where
		tones1 = tones past; tones2 = tones ch
		jumps = map (toneJumpFrom tones1) tones2
		avg = fromIntegral (sum jumps) / genericLength tones2
		maxJump = maximum jumps

chanceInScale ch _ = if all (\t -> isToneFromScale t key1 intervals1) tones1
	then 1 else floatMin
	where tones1 = tones ch; key1 = key ch; intervals1 = intervals ch

chanceTriad ch _ = if null tones1 || isFullTriad tones1 key1 intervals1
	then 1 else floatMin
	where tones1 = tones ch; key1 = key ch; intervals1 = intervals ch

chanceTonicStart ch [] = if isTonic tones1 key1 intervals1 then 1 else 0
	where tones1 = tones ch; key1 = key ch; intervals1 = intervals ch
chanceTonicStart _ _ = 1

chanceNotDomThenSub ch (past:_) = if isDominant tones1 key1 intervals1 &&
	isSubdominant tones2 key2 intervals2 then floatZero else 1
	where tones1 = tones past; key1 = key past; intervals1 = intervals past;
		tones2 = tones ch; key2 = key ch; intervals2 = intervals ch
chanceNotDomThenSub _ [] = 1

chanceLeadingTone ch (past:_) = if isLeadingToneOk tones1 tones2 key2 intervals2
	then 1 else 0
	where tones1 = tones past; tones2 = tones ch; key2 = key ch;
		intervals2 = intervals ch
chanceLeadingTone ch [] = if isLeadingToneOk [] tones1 key1 intervals1
	then 1 else 0
	where tones1 = tones ch; key1 = key ch; intervals1 = intervals ch

chanceNotEmpty ch _ = if tones ch == [] then floatMin else 1

chance4Tones ch _ = if length (nub $ tones ch) == 4 then 1 else floatMin

chanceCounterpoint ch (past:_) = if isCounterpoint tones1 tones2
	then 1 else floatMin
	where tones1 = tones past; tones2 = tones ch
chanceCounterpoint _ [] = 1

chanceMove ch (past:_)
	| sopMv && bassMv && pct > 0.5 = 1
	| sopMv && bassMv = 0.8
	| sopMv || bassMv = 0.6
	| pct > 0.5 = 0.5
	| otherwise = floatMin
	where
		tones1 = tones past; tones2 = tones ch
		sopMv = isSopranoMoving tones1 tones2
		bassMv = isBassMoving tones1 tones2
		pct = percentageMoving tones1 tones2
chanceMove _ [] = 1

