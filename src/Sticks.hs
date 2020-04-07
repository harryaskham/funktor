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

-- delayed for a slow psy intro
-- metal rhythim for a drop
-- other metal rhythem for fills

root = D

song :: SongM
song = do
  gBPM <- asks (view bpm)

  sam1 <- toSeg <$> loadSample "samples/Basic Bashing.wav"
  sam2 <- toSeg <$> loadSample "samples/Chord Jangle.wav"
  sam3 <- toSeg <$> loadSample "samples/Delay Muted.wav"
  sam4 <- toSeg <$> loadSample "samples/Echoplex.wav"
  sam5 <- toSeg <$> loadSample "samples/Metal Rhythm 1.wav"
  sam6 <- toSeg <$> loadSample "samples/Metal Rhythm 2.wav"
  sam7 <- toSeg <$> loadSample "samples/Rootsy.wav"
  sam8 <- toSeg <$> loadSample "samples/Sitar Guitar.wav"
  sam9 <- toSeg <$> loadSample "samples/Slow Wah.wav"
  return $ mel $ constLim 4 <$> [sam1, sam2, sam3, sam4, sam5, sam6, sam7, sam8, sam9]

songEnv = SongEnv { _bpm=128
                  , _beatLength=1024
                  }

sts' = runSongM songEnv song
sts = dac =<< sts'
