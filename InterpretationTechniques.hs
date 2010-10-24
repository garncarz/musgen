module InterpretationTechniques where

import MGRandom
import Random
import Relations
import Types

sopranoFlow :: RndGen -> Flow -> Flow
sopranoFlow _ [] = []
sopranoFlow gen flow = ch2 : sopranoFlow gen rest where
	ch1:rest = flow; soprano = maximum $ tones ch1
	ch2 = ch1 {tones = [soprano]}

randomMelodyFlow :: RndGen -> Flow -> Flow
randomMelodyFlow _ [] = []
randomMelodyFlow gen flow = ch2 : randomMelodyFlow g2 rest where
	ch1:rest = flow; tones1 = tones ch1
	(rndPos, g2) = randomR (0, length tones1 - 1) gen
	tone = tones1 !! rndPos; ch2 = ch1 {tones = [tone]}

fingeredChord :: Chord -> Int -> Flow
fingeredChord ch step = if dur1 <= 0 then []
	else ch2 : fingeredChord chRest (step + 1)
	where
		tones1 = tones ch; dur1 = dur ch; remain1 = remain ch
		beat = measure ch `div` beats ch
		nextBeat = (remain1 - 1) `div` beat * beat
		dur2 = if remain1 - dur1 > nextBeat then dur1 else remain1 - nextBeat

		chTone i = tones1 !! (if i < length tones1
			then i else length tones1 - 1)
		pos = if step == 0 then 0 else if odd step then 2 else 1

		ch2 = ch {tones = [chTone pos], dur = dur2}
		chRest = ch {dur = dur1 - dur2}
		-- remain od chRest neupraveno záměrně pro urychlení hry

fingeredFlow :: Flow -> Flow
fingeredFlow [] = []
fingeredFlow flow = fingeredChord ch 0 ++ fingeredFlow rest
	where (ch:rest) = flow

chordRhythmFlow :: Flow -> Flow
chordRhythmFlow [] = []
chordRhythmFlow flow = ch1 : chordRhythmFlow flow2 where
	ch:rest = flow
	remain1 = remain ch; dur1 = dur ch
	beat = measure ch `div` beats ch
	nextBeat = (remain1 - 1) `div` beat * beat
	overlap = (remain1 - dur1) < nextBeat
	ch1 = if overlap then ch {dur = dur2} else ch
	dur2 = remain1 - nextBeat
	flow2 = if overlap then ch2:rest else rest
	ch2 = ch {dur = dur1 - dur2, begin = begin ch + dur2}

octaveShift :: Int -> Flow -> Flow
octaveShift shift =
	map (\ch -> ch {tones = map (+ scaleSize * shift) $ tones ch})

