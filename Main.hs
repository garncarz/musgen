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
	new :: Bool} deriving (Data, Typeable, Show)

use :: Mode Input
use = mode $ Input {
	key = def &=
		text "Key of harmony" & typ "MIDI_TONE" & empty "random",
	scale = def &=
		text "Scale of harmony" & typ "major|minor" & empty "random",
	beats = def &=
		text "Beats per measure" & typ "INT" & empty "random",
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
			beats = argValue beats "4"}
			where argValue arg implicit =
				if arg input /= "" then arg input else implicit
		
		chKey = read (key input2)
		chScale = scale input2
		chBeats = read (beats input2)
		chMeasure = chBeats * 4
		
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
	
	flowExists <- doesFileExist "flow.txt"
	flow <- if flowExists && not (new input) then loadFlow
		else produceFlow startChord
	exportMidi "song.midi" $ midiFile [harmonyTrack flow, sopranoTrack flow,
		bassTrack flow, harmonyRhythmTrack flow]
	putStrLn "MIDI generated."

