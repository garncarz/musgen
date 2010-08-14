{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import Control.Exception
import Flow
import InstrumentBass
import InstrumentHarmony
import InstrumentHarmonyRhythm
import InstrumentSoprano
import Midi
import MGRandom
import Prelude hiding (catch)
import Random
import Relations
import System
import System.Console.CmdArgs
import System.Directory
import qualified Types as T

data Input = Input {key :: String, scale :: String, beats :: String,
	minMeasures :: String, new :: Bool} deriving (Data, Typeable, Show)

use :: Mode Input
use = mode $ Input {
	key = def &=
		text "Key of harmony" & typ "MIDI_TONE" & empty "random",
	scale = def &=
		text "Scale of harmony" & typ "major|minor" & empty "random",
	beats = def &=
		text "Beats per measure" & typ "INT" & empty "4",
	minMeasures = def &=
		text "Minimal number of measures" & typ "INT" & empty "20",
	new = def &=
		text "Generate new flow"}

checkArg :: a -> String -> IO ()
checkArg arg name = do
	evaluate arg `catch` (\(ErrorCall _) -> do
		putStrLn $ "Wrong format for " ++ name
		exitFailure)
	return ()

main :: IO ()
main = do
	input <- cmdArgs "MusGen" [use]
	gen <- newStdGen
	
	let
		g = rndSplitL gen
		input2 = input {
			key = argValue key (let (i, _) = randomR (-6 :: Int, 6) (g !! 0)
				in show $ i + 76),
			scale = argValue scale (let (i, _) = randomR (0, 1) (g !! 1)
				in ["major", "minor"] !! i),
			beats = argValue beats "4",
			minMeasures = argValue minMeasures "20"}
			where argValue arg implicit =
				if arg input /= "" then arg input else implicit
		
		chKey = read $ key input2
		chScale = scale input2
		chBeats = read $ beats input2
		chMeasure = chBeats * 4
		flMinMeasures = read $ minMeasures input2
		
		startChord = T.Chord {T.tones = [], T.key = chKey,
			T.intervals = if chScale == "minor" then minor else major,
			T.dur = 0, T.measure = chMeasure, T.remain = chMeasure,
			T.beats = chBeats}
	
	checkArg chKey "key"
	if not (elem chScale ["major", "minor"]) then do
		putStrLn "Scale can be just major or minor"
		exitFailure
		else return ()
	checkArg chBeats "beats"
	checkArg flMinMeasures "minMeasures"
	
	flowExists <- doesFileExist "flow.txt"
	flow <- if flowExists && not (new input) then loadFlow
		else produceFlow startChord flMinMeasures
	exportMidi "song.midi" $ midiFile [
		harmonyTrack flow (g !! 2),
		sopranoTrack flow (g !! 3),
		bassTrack flow (g !! 4),
		harmonyRhythmTrack flow (g !! 5)]
	putStrLn "MIDI generated."

