module Main where

-- TODO načítání či znovu produkování Flow podle argumentu

import Flow
import InstrumentBass
import InstrumentHarmony
import InstrumentHarmonyRhythm
import InstrumentSoprano
import Midi
import System.Directory

main :: IO ()
main = do
	flowExists <- doesFileExist "flow.txt"
	flow <- if flowExists then loadFlow else produceFlow
	putStrLn "Flow loaded."
	exportMidi "song.midi" $ midiFile [harmonyTrack flow, sopranoTrack flow,
		bassTrack flow, harmonyRhythmTrack flow]

