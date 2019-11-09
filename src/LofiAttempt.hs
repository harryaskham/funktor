module LofiAttempt where

import Csound.Base hiding (Tab, clp)
import qualified Csound.Catalog.Drum.Tr808 as Tr808
import qualified Csound.Catalog.Drum.Hm as Hm
import Csound.Patch
import Csound.Sam
import Csound.Sam.Core
import Data.List.Split
import Tabs
import Tools
import Note
import Melody
import Data.Functor ((<&>))
import System.Random
import Control.Monad

-- Nice slo lofi bpm
bpm = 70

numBeats :: Int
numBeats = 256

-- Remove 10% of the beats and incorporate pink noise into the samples.
drumFx = (* pink2)
compile t = drumFx <$> compileWithDropOut 0.1 bpm t

bd1 = compile $ DrumTab "O _ _ _ _ _ _ _|_ _ _ _ _ _ o _|_ _ _ _ _ _ _ _|_ _ o _ _ _ _ _" Hm.bd1 numBeats
sn1 = compile $ DrumTab "_ _ _ _ _ _ _ _|o _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|o _ _ _ _ _ _ _" Hm.sn1 numBeats
chh = compile $ DrumTab "_ _ _ _ _ _ o _|_ _ _ _ o _ _ _|o _ _ _ o _ o _|_ _ _ _ X _ _ _" Hm.chh numBeats

-- Weights for picking notes out of the minor scale
weights = [5, 3, 3, 2, 3, 3, 2] 

-- Pick a random minor chord from the scale every bar.
chords :: Note -> IO TrackSegment
chords root = do
  g <- newStdGen
  return
    $ Segment bpm epiano1
    $ mel . fmap makeChord
    $ rndFrom g numBeats (minorChords root)
  where
    makeChord ch = toChord $ Pch <$> ch ?? 7 ?? 0.6 ?? bars 1

-- Hit 50% of beats from the scale on the banyan
lead :: Note -> IO TrackSegment
lead root = do
  g <- newStdGen
  return
    $ Segment bpm banyan
    $ toMel $ rndFrom g numBeats 
    $ notes
    <*> [7, 7, 8, 8, 8, 9, 9]
    <*> [0.85, 0.0]
    <*> [1]
  where
    notes = weightsToPchs $ zip (minorScale root) weights

-- A 4-bar motif that will loop underneath
motif :: Note -> IO TrackSegment
motif root = do
  g <- newStdGen
  return
    $ Segment bpm epiano1
    $ toMel . getZipList
    $ ZipList (take numBeats . cycle $ rndFrom g 7 noteGen)
    <*> ZipList (cycle [4, 4, 1, 1, 1, 0.5, 4.5])
  where
    notes = weightsToPchs $ zip (minorScale root) weights
    noteGen = notes
      <*> [7, 7, 8, 8, 8, 9, 9]
      <*> [1.0]

-- TODO: A wavetable version that lets us have uneven on/off

song' :: Note -> IO Song
song' root = Song bpm <$> sequenceA (drumSegments ++ instrSegments)
  where
    drumSegments =
      [ EnvDrums <$> bd1 ?? constEnv
      , EnvDrums <$> sn1 ?? constEnv
      , EnvDrums <$> chh ?? constEnv ]
    instrSegments = genEnvSegs [chords, lead, motif] root constEnv

-- Procedurally generated lofi
song :: IO (SE Sig2)
song = compileSong <$> song' Fs

-- Generator for a looping 16bar lofi, more hiphop
-- Needs numBeats turning down to work properly
songLoop :: IO (SE Sig2)
songLoop = do
  fsSong <- compileSong <$> song' Fs
  aSong <- compileSong <$> song' A
  return $ do
    fsSeg <- toSeg <$> fsSong
    aSeg <- toSeg <$> aSong
    return $ runSeg $ loop $ mel [constLim (beatsToSecs $ Beats bpm 4) fsSeg, constLim (beatsToSecs $ Beats bpm 4) aSeg]
