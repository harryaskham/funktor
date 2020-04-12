{-# LANGUAGE FlexibleContexts #-}

module SticksThree where

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

  kcks <- drums "X _ _ _|O _ _ _|O _ _ _|O _ _ _" Tr808.bd2
  chhs <- drums "X _ o _|O O _ _|X O o .|_ _ X _" Hm.chh
  ohhs <- drums "_ . _ _|_ _ O _|_ _ _ _|O O _ _" Hm.ohh
  snrs <- drums "_ _ X _|_ o _ _|" Hm.sn1
  let drums = smallRoom2 <$> har [kcks, chhs, ohhs, snrs]
  let guitar = loop $ constLim (beatsToSecs $ Beats gBPM 16) $ toSeg $ scaleWav 0 0.9 1 "samples/Echoplex.wav"

  return $ har [drums, guitar]

songEnv = SongEnv { _bpm=128
                  , _beatLength=128
                  }

st3' = runSongM songEnv song
st3 = dac =<< st3'
