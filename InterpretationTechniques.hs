module InterpretationTechniques where

import MGRandom
import Random
import Relations
import Types

-- TODO pravděpodobně úplně odstranit vycházení mimo harmonii
melodyFlow :: RndGen -> Flow -> Flow
melodyFlow _ [] = []
melodyFlow gen flow = chs ++ melodyFlow (g !! 0) rest
	where
		(ch1:rest) = flow; tone1 = head $ tones ch1; dur1 = dur ch1
		key1 = key ch1; intervals1 = intervals ch1
		(ch2:_) = rest; tone2 = head $ tones ch2
		beat = measure ch1 `div` beats ch1
		
		tone1Up = succToneIn key1 intervals1 tone1
		tone1Down = predToneIn key1 intervals1 tone1
		tone2Up = succToneIn key1 intervals1 tone2
		tone2Down = predToneIn key1 intervals1 tone2
		
		tone1UpLead = isLeadingToneIn key1 intervals1 tone1Up
		tone1DownLead = isLeadingToneIn key1 intervals1 tone1Down
		
		chs = {- if dur1 > beat && not tone1UpLead && (yes !! 0) then [
			ch1 {tones = [tone1], dur = dur1 `div` 3},
			ch1 {tones = [tone1Up], dur = dur1 `div` 3},
			ch1 {tones = [tone1], dur = dur1 - 2 * (dur1 `div` 3)}]
			else if dur1 > beat && (yes !! 1) then [
			ch1 {tones = [tone1], dur = dur1 `div` 3},
			ch1 {tones = [tone1Down], dur = dur1 `div` 3},
			ch1 {tones = [tone1], dur = dur1 - 2 * (dur1 `div` 3)}]
			else -} if length flow > 1 && tone1Up == tone2Down && (yes !! 2) then [
			ch1 {tones = [tone1], dur = dur1 `div` 2},
			ch1 {tones = [tone1Up], dur = dur1 - (dur1 `div` 2)}]
			else if length flow > 1 && tone1Down == tone2Up && not tone1DownLead
			&& (yes !! 3) then [
			ch1 {tones = [tone1], dur = dur1 `div` 2},
			ch1 {tones = [tone1Down], dur = dur1 - (dur1 `div` 2)}]
			else [ch1]
		
		yes = randomRs (True, False) (g !! 1)
		g = rndSplitL gen

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
	(ch:rest) = flow
	remain1 = remain ch; dur1 = dur ch
	beat = measure ch `div` beats ch
	nextBeat = (remain1 - 1) `div` beat * beat
	overlap = (remain1 - dur1) < nextBeat
	ch1 = if overlap then ch {dur = dur2} else ch
	dur2 = remain1 - nextBeat
	flow2 = if overlap then ch2:rest else rest
	ch2 = ch {dur = dur1 - dur2, remain = nextBeat}

octaveShift :: Int -> Flow -> Flow
octaveShift shift =
	map (\ch -> ch {tones = map (+ scaleSize * shift) $ tones ch})

