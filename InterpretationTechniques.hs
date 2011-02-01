module InterpretationTechniques where

import MGRandom
import Random
import Relations
import Types

octaveShift :: Int -> Flow -> Flow
octaveShift shift =
	map (\ch -> ch {tones = map (+ scaleSize * shift) $ tones ch})

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

fingeredFlow :: RndGen -> Flow -> Flow
fingeredFlow _ [] = []
fingeredFlow gen flow = fingeredChord ch 0 ++ fingeredFlow gen rest where
	(ch:rest) = flow
	fingeredChord :: Chord -> Int -> Flow
	fingeredChord ch step = if dur1 <= 0 then []
		else ch2 : fingeredChord chRest (step + 1)
		where
			tones1 = tones ch; dur1 = dur ch; remain1 = remain ch
			beat = measure ch `div` beats ch
			nextBeat = (remain1 - 1) `div` beat * beat
			dur2 = if remain1 - dur1 > nextBeat then dur1 else remain1 -
				nextBeat

			chTone i = tones1 !! (if i < length tones1
				then i else length tones1 - 1)
			pos = if step == 0 then 0 else if odd step then 2 else 1

			ch2 = ch {tones = [chTone pos], dur = dur2}
			chRest = ch {dur = dur1 - dur2, begin = begin ch2 + dur ch2}
			-- remain od chRest neupraveno záměrně pro urychlení hry

chordRhythmFlow :: RndGen -> Flow -> Flow
chordRhythmFlow _ [] = []
chordRhythmFlow gen flow = ch1 : chordRhythmFlow gen flow2 where
	ch:rest = flow
	remain1 = remain ch; dur1 = dur ch
	beat = measure ch `div` beats ch
	nextBeat = (remain1 - 1) `div` beat * beat
	overlap = (remain1 - dur1) < nextBeat
	ch1 = if overlap then ch {dur = dur2} else ch
	dur2 = remain1 - nextBeat
	flow2 = if overlap then ch2:rest else rest
	ch2 = ch {dur = dur1 - dur2, begin = begin ch + dur2}

brokenChord1_5_10 :: RndGen -> Flow -> Flow
brokenChord1_5_10 gen flow = brokenChord1_5_10inner gen flow 0 where
	brokenChord1_5_10inner _ [] _ = []
	brokenChord1_5_10inner gen flow step = ch1 :
		brokenChord1_5_10inner gen flow2 step2
		where
		ch:rest = flow
		remain1 = remain ch; dur1 = dur ch
		beat = measure ch `div` beats ch
		nextBeat = (remain1 - 1) `div` beat * beat
		overlap = (remain1 - dur1) < nextBeat
		tones1 = if even step then [tones ch !! 0, tones ch !! 2]
			else [tones ch !! 1 + scaleSize]
		ch1 = if overlap then ch {tones = tones1, dur = dur2}
			else ch {tones = tones1}
		dur2 = remain1 - nextBeat
		flow2 = if overlap then ch2:rest else rest
		ch2 = ch {dur = dur1 - dur2, begin = begin ch + dur2}
		step2 = if overlap then step + 1 else 0

walkingBass :: RndGen -> Flow -> Flow
walkingBass gen flow = walkingBassInner gen flow 0 where
	walkingDur = 2
	walkingBassInner _ [] _ = []
	walkingBassInner gen flow lastTone = ch1 :
		walkingBassInner gen flow2 lastTone2
		where
		ch:rest = flow; dur1 = dur ch
		
		endTone = if length rest > 0 then tones (rest !! 0) !! 0
			else tones ch !! 0
		toneUp = succToneIn (key ch) (intervals ch) lastTone
		toneDown = predToneIn (key ch) (intervals ch) lastTone
		tone = if lastTone == 0 then tones ch !! 0
			else if abs (toneUp - endTone) < abs (toneDown - endTone) &&
				abs (toneUp - endTone) > 0 then toneUp else toneDown
		
		ch1 = ch {tones = [tone], dur = min walkingDur $ dur ch}
		ch2 = ch {begin = begin ch + dur ch1, dur = dur ch - dur ch1}
		flow2 = if dur ch2 > 0 then ch2:rest else rest
		lastTone2 = if dur ch2 > 0 then tone else 0
		
