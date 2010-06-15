module Pitch where

import List
import Maybe

type Tone = String
type Octave = Int
data Pitch = Pitch Tone Octave deriving (Eq)

instance Show Pitch where
	show (Pitch t o) = t ++ (show o)

instance Ord Pitch where
	compare (Pitch t1 o1) (Pitch t2 o2)
		| o1 < o2 = LT
		| o1 == o2 && pos1 < pos2 = LT
		| o1 == o2 && pos1 == pos2 = EQ
		| otherwise = GT
		where
			pos1 = fromJust (elemIndex t1 tones)
			pos2 = fromJust (elemIndex t2 tones)

instance Enum Pitch where
	succ (Pitch t1 o1) = Pitch t2 o2 where
		pos1 = fromJust (elemIndex t1 tones)
		pos2
			| pos1 - (length tones) == -1 = 0
			| otherwise = pos1 + 1
		t2 = tones !! pos2
		o2
			| pos2 == 0 = o1 + 1
			| otherwise = o1
	pred (Pitch t1 o1) = Pitch t2 o2 where
		pos1 = fromJust (elemIndex t1 tones)
		pos2
			| pos1 == 0 = (length tones) - 1
			| otherwise = pos1 - 1
		t2 = tones !! pos2
		o2
			| pos1 == 0 = o1 - 1
			| otherwise = o1

addPitch :: Pitch -> Int -> Pitch
addPitch p 0 = p
addPitch p add
	| add > 0 = addPitch (succ p) (add - 1)
	| add < 0 = addPitch (pred p) (add + 1)


tones :: [Tone]
tones = ["c", "c#", "d", "d#", "e", "f", "f#", "g", "g#", "a", "a#", "b"]

