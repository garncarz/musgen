{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import Control.Exception
import Control.Monad
import Flow
import Interpretation
import MGRandom
import Midi
import Prelude hiding (catch)
import Random
import Relations
import System
import System.Console.CmdArgs hiding (name)
import System.Directory
import qualified Types as T
import Var

data Input = Input {key :: String, scale :: String, beats :: String,
	tempo :: String, minMeasures :: String, interpretation :: String,
	new :: Bool, name :: String} deriving (Data, Typeable, Show)

use = Input {
	key = def &=
		help "Key of harmony" &= typ "MIDI_TONE" &= opt "random",
	scale = def &=
		help "Scale of harmony" &= typ "major|minor" &= opt "random",
	beats = def &=
		help "Beats per measure" &= typ "INT" &= opt "4",
	tempo = def &=
		help "Quarter notes per minute" &= typ "INT" &= opt "120",
	minMeasures = def &=
		help "Minimal number of measures" &= typ "INT" &= opt "20",
	interpretation = def &=
		help "Style of interpretation" &= typ "STYLE" &= opt
		(fst $ interpretations !! 0),
	new = def &= help "Generate new flow",
	name = def &= typ "SONG_NAME" &= opt "song" &= argPos 0 }
	&= program "musgen"
	&= summary ("MusGen, version date: " ++ versionDate)
	&= details [
		"Generates a song fulfilling given parameters, " ++
		"song's name is \"song\" by default. Available styles of " ++
		"interpretation are: " ++ (init . init . concat $
		map (\i -> fst i ++ ", ") interpretations) ++ ".",
		"Output is SONG_NAME.midi with playable MIDI data " ++
		"and SONG_NAME.flow with reusable information about harmony flow."]

checkArg :: a -> String -> IO ()
checkArg arg name = do
	evaluate arg `catch` (\(ErrorCall _) -> do
		putStrLn $ "Wrong format for " ++ name
		exitFailure)
	return ()

main :: IO ()
main = do
	input <- cmdArgs use
	gen <- newStdGen
	
	let
		g = rndSplitL gen
		input2 = input {
			key = argValue key (let (i, _) = randomR (-6 :: Int, 6) (g !! 0)
				in show $ i + 76),
			scale = argValue scale (let (i, _) = randomR (0, 1) (g !! 1)
				in ["major", "minor"] !! i),
			beats = argValue beats "4",
			tempo = argValue tempo "120",
			minMeasures = argValue minMeasures "20",
			interpretation = argValue interpretation $ fst $
				interpretations !! 0,
			name = argValue name "song" }
			where argValue arg implicit =
				if arg input /= "" then arg input else implicit
		chKey = read $ key input2
		chScale = scale input2
		chBeats = read $ beats input2
		midiTempo = 60000000 `div` read (tempo input2)
		chMeasure = chBeats * 4
		flMinMeasures = read $ minMeasures input2
		flInterpretation = interpretation input2
	
	checkArg chKey "key"
	when (chScale `notElem` ["major", "minor"]) $ do
		putStrLn "Scale can be just major or minor"
		exitFailure
	checkArg chBeats "beats"
	checkArg midiTempo "tempo"
	checkArg flMinMeasures "minMeasures"
	when (flInterpretation `notElem` map fst interpretations) $
		do putStrLn "Invalid style of interpretation"; exitFailure
	
	let
		flowFilename = name input2 ++ ".flow"
		midiFilename = name input2 ++ ".midi"
		startChord = T.Chord {T.tones = [], T.key = chKey,
			T.intervals = if chScale == "minor" then minor else major,
			T.dur = 0, T.measure = chMeasure, T.begin = 0, T.beats = chBeats}
		tracks = makeTracks (findTracks flInterpretation interpretations) where
			findTracks iName ((iName2, tracksDefs):rest)
				| iName == iName2 = tracksDefs
				| otherwise = findTracks iName rest
	
	flowExists <- doesFileExist flowFilename
	flow <- if flowExists && not (new input) then loadFlow flowFilename
		else produceFlow startChord flMinMeasures flowFilename
	exportMidi midiFilename $ midiFile $ tracks flow (g !! 2) midiTempo
	putStrLn "MIDI generated."

