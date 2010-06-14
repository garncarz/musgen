module Pitch where

import List
import Maybe

type Tone = String
type Octave = Int
data Pitch = Pitch Tone Octave deriving (Eq, Show)

instance Ord Pitch where
	compare (Pitch t1 o1) (Pitch t2 o2)
		| o1 < o2 = LT
		| o1 == o2 && p1 < p2 = LT
		| o1 == o2 && p1 == p2 = EQ
		| otherwise = GT
		where
			p1 = fromJust (elemIndex t1 tones)
			p2 = fromJust (elemIndex t2 tones)

instance Enum Pitch where
	succ (Pitch t1 o1) = Pitch t2 o2 where
		p1 = fromJust (elemIndex t1 tones)
		p2
			| p1 - (length tones) == -1 = 0
			| otherwise = p1 + 1
		t2 = tones !! p2
		o2
			| p2 == 0 = o1 + 1
			| otherwise = o1
	pred (Pitch t1 o1) = Pitch t2 o2 where
		p1 = fromJust (elemIndex t1 tones)
		p2
			| p1 == 0 = (length tones) - 1
			| otherwise = p1 - 1
		t2 = tones !! p2
		o2
			| p1 == 0 = o1 - 1
			| otherwise = o1

tones :: [Tone]
tones = ["c", "cis", "d", "dis", "e", "f", "fis", "g", "gis", "a", "ais", "b"]

