{-# LANGUAGE FlexibleContexts #-}

module Sticks where

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
  gBPM <- asks (view bpm)

  sam1 <- toSeg <$> loadSample "samples/Basic Bashing.wav"
  sam2 <- toSeg <$> loadSample "samples/Chord Jangle.wav"
  sam4 <- toSeg <$> loadSample "samples/Echoplex.wav"
  sam5 <- toSeg <$> loadSample "samples/Metal Rhythm 1.wav"
  sam6 <- toSeg <$> loadSample "samples/Metal Rhythm 2.wav"
  sam7 <- toSeg <$> loadSample "samples/Rootsy.wav"
  sam8 <- toSeg <$> loadSample "samples/Sitar Guitar.wav"
  sam9 <- toSeg <$> loadSample "samples/Slow Wah.wav"

  intro1 <- do
    kicks <- drums "X|O|O|O" Tr808.bd2
    wobble <- do
      i <- compileI nightPad (Pch <$> [D, F, A, Fs] <*> pure 8 <*> pure 0.5 <*> pure 4)
      e <- sqrTabEnv [OnFor (1/2), OffFor (1/2)]
      return $ stereoMap (e*) <$> i
    drop <- do
      i <- compileI razorLead (Pch <$> [A, Fs, D, F] <*> pure 9 <*> pure 0.5 <*> pure (1/4))
      e <- sqrTabEnv [OffFor 28, OnFor 4]
      return $ stereoMap (e*) <$> i
    let guitar = loop $ mul 1.3 $ constLim (beatsToSecs $ Beats gBPM 8) $ toSeg $ scaleWav 0 1.15 1 "samples/Delay Muted.wav"
    return $ har [kicks, guitar, wobble, drop]

  intro2 <- do
    drms <- do
      kicks <- drums "X _ _ _|O _ _ _|O _ _ _|O _ _ _" Tr808.bd2
      snars <- drums "_ _ _ _|X _ _ _|_ _ _ _|X _ _ _" Hm.clap
      chhhs <- drums "O o . ." Tr808.chh
      ohhhs <- drums "_ _ X _" Tr808.ohh
      return $ har [kicks, snars, chhhs, ohhhs]
    bass <- compileI fmBass1 [Pch D 7 0.5 (1/2), Silent (1/2)]
    wobble <- do
      i <- compileI nightPad (Pch <$> [D, F, A, Fs] <*> pure 8 <*> pure 0.5 <*> pure 2)
      e <- sqrTabEnv [OnFor (1/2), OffFor (1/2)]
      return $ stereoMap (e*) <$> i
    drop <- do
      i <- compileI razorLead (Pch <$> [A, Fs, D, F] <*> pure 9 <*> pure 0.5 <*> pure (1/4))
      e <- sqrTabEnv [OffFor 28, OnFor 4]
      return $ stereoMap (e*) <$> i
    let guitar = loop $ mul 1.3 $ constLim (beatsToSecs $ Beats gBPM 32) $ toSeg $ scaleWav 0 1.15 1 "samples/Delay Muted.wav"
    return $ har [drms, bass, guitar, wobble, drop]

  fill <- do
    drms <- do
      kicks <- drums "X _ _ _|O _ _ _|O _ _ _|O _ _ _" Tr808.bd2
      chhhs <- drums "O o . ." Tr808.chh
      return $ har [kicks, chhhs]
    bass <- compileI fmBass1 [Pch D 7 0.5 (1/2), Silent (1/2)]
    return $ har [drms, bass]

  v1 <- do
    drms <- do
      kicks <- drums "X _ _ _|O _ _ _|O _ _ _|O _ _ _" Tr808.bd2
      snars <- drums "_ _ _ _|X _ _ _|_ _ _ _|X _ _ _" Hm.clap
      chhhs <- drums "O o . ." Tr808.chh
      ohhhs <- drums "_ _ o _" Tr808.ohh
      return $ har [kicks, snars, ohhhs, chhhs]
    bass <- compileI fmBass1 [Pch D 7 0.5 (1/2), Silent (1/2)]
    wobble <- do
      i <- compileI nightPad (Pch <$> [A, F, D, Fs] <*> pure 8 <*> pure 0.6 <*> pure 2)
      e <- sqrTabEnv [OnFor (1/8), OffFor (1/8)]
      return $ stereoMap (e*) <$> i
    drop <- do
      i <- compileI razorLead (Pch <$> [A, Fs, D, F] <*> pure 9 <*> pure 0.5 <*> pure (1/4))
      e <- sqrTabEnv [OffFor 28, OnFor 4]
      return $ stereoMap (e*) <$> i
    let guitar = loop $ mul 1.2 $ constLim (beatsToSecs $ Beats gBPM 8) $ toSeg $ scaleWav 0 1 1 "samples/MetalRhythm2.wav"
    return $ har [drms, bass, guitar, wobble, drop]

  {-
  e1 <- sqrTabEnv [OnFor 32, OffFor 1000]
  e2 <- sqrTabEnv [OffFor 32, OnFor 32, OffFor 1000]
  e3 <- sqrTabEnv [OffFor 64, OnFor 8, OffFor 1000]
  e4 <- sqrTabEnv [OffFor 72, OnFor 32, OffFor 1000]
  return $ har [ stereoMap (*e1) <$> intro1
               , stereoMap (*e2) <$> intro2
               , stereoMap (*e3) <$> fill
               , stereoMap (*e4) <$> v1
               ]
               -}

  forBeats 4 $ har [intro1, intro2, fill, v1]
    {-
  cotraverse mel [ forBeats 4 intro1
                 , forBeats 4 intro2
                 , forBeats 4 fill
                 , forBeats 4 v1
                 ]
                 -}

songEnv = SongEnv { _bpm=150
                  , _beatLength=1024
                  }

sts' = runSongM songEnv song
sts = dac =<< sts'
