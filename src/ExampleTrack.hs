{-# LANGUAGE FlexibleContexts #-}

module ExampleTrack where

import Csound.Base hiding (Tab, clp, Duration)
import Csound.Sam
import Csound.Patch
import Tabs
import Tools
import Note
import Melody
import Sample
import Data.Sort
import Data.Ord
import Control.Lens
import Csound.Catalog.Drum.Tr808 as Tr808
import Csound.Catalog.Drum.Hm as Hm
import Csound.Catalog.Effect
import System.IO.Unsafe
import Control.Monad.Reader
import Control.Monad.Random
import Data.List
import System.Random

song :: SongM
song = do
  -- 4 on the floor bass drum, on for 16 beats, off for 16 beats
  kicks <- do
    d <- drums "X _ _ _|O _ _ _|O _ _ _|O _ _ _" Tr808.bd2
    env <- sqrEnvM 0 16
    return (d & withEnv env)

  -- Closed hats, always on
  cHats <- drums "X O o ." Tr808.chh

  -- Open hats, always on
  oHats <- drums "o _ o _|_ _ o _" Tr808.ohh

  -- Group the durms
  let drms = har [kicks, cHats, oHats]

  -- Bassline - 5 notes from two chords at 2/3 velocity every 4 beats
  -- Oscillates in and out on a sinewave every 16 beats
  bass <- do
    let notes = take 5 $ minorChord D ++ reverse (majorChord Bb)
    compileI (withDeepBass 1.0 fmBass1) $ Pch <$> notes ?? 6 ?? (2/3) ?? 4

  -- Take 16 random notes from D minor, over 3 octaves, with some notes silent.
  lead <- do
    notes <-
      sequence
      $ replicate 16 randomFrom
      ?? expandScale [7, 8, 9] (minorScale D) ++ replicate 10 (\_ _ -> Silent (1/2))
    compileI razorLead $ notes ?? (1/2) ?? (1/2)

  -- Add some reverb FX to the instrs
  let instrs = har [ rever2 0.5 <$> bass
                   , rever2 0.2 <$> lead
                   ]

  -- Play all the above at once
  return $ drms =:= instrs

-- Set BPM and track length
songEnv :: SongEnv
songEnv = SongEnv { _bpm=140
                  , _beatLength=64
                  }

-- Play the song in real time
et = dac =<< runSongM songEnv song
diskEt = runToDisk =<< runSongM songEnv song

