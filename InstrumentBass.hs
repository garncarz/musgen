module InstrumentBass (bassTrack) where

import Interpretation
import Midi
import Random
import Relations
import Types

lowerFlow :: Flow -> Flow
lowerFlow = map (\ch -> ch {tones = map (subtract (scaleSize * 2)) $ tones ch})

bassTrack :: RandomGen g => Flow -> g -> MidiTrack
bassTrack flow _ = midiTrack 3 "Bass" 33
	(flow2Midi . fingeredFlow $ lowerFlow flow) (flow !! 0)

