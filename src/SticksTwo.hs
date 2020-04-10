{-# LANGUAGE FlexibleContexts #-}

module SticksTwo where

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

speedMod = 1.0

song :: SongM
song = do
  gBPM <- asks (view bpm)

  kcks <- drums "X _ _ _|O _ _ _|O _ _ _|O _ _ _" Tr808.bd2
  chhs <- drums "_ o _ .|_ o _ .|_ o _ .|_ o _ ." Tr808.chh
  ohhs <- drums "_ _ o _|_ _ o _|_ _ o _|_ _ o _" Tr808.ohh
  cyms <- drums "_ _ _ _|_ _ _ _|_ _ _ _|X _ _ _" Tr808.cym
  clps <- drums "_ _ _ _|O _ _ _|_ _ _ _|o _ _ _" Hm.sn1

  let guitar = loop $ mul 0.9 $ constLim (beatsToSecs $ Beats gBPM 8) $ toSeg $ scaleWav 0 (1.058*speedMod) 1 "samples/SitarGuitar.wav"

  wobble <- do
    i <- compileI razorLead $ Pch <$> [D, A, D, F] <*> pure 9 <*> pure 0.5 <*> pure 4
    e <- sinEnvM 0 (1/6)
    return $ stereoMap (*e) <$> i

  arp <- do
    let notes = expandScale [7, 8] (minorScale D) <*> pure 0.5 <*> pure (1/2)
        ns = takeIxs [0, 4, 4, 3, 2, 1, 6, 6, 6, 7, 8, 7, 12, 12, 13, 12] notes
    compileI banyan ns

  return
    $ mel
    $ constLim (beatsToSecs $ Beats gBPM 16)
    <$>
    [ har [arp, kcks]
    , har [guitar, arp, kcks]
    , har [guitar, arp, kcks, ohhs]
    , har [guitar, arp, kcks, ohhs, wobble]
    , har [arp, kcks, ohhs, wobble, chhs]
    , har [kcks, chhs, ohhs, cyms, clps, guitar, wobble, arp]
    , har [guitar, arp, kcks]
    , har [arp]
    ] 
songEnv = SongEnv { _bpm=speedMod*128
                  , _beatLength=1024
                  }

st2' = runSongM songEnv song
st2 = dac =<< st2'
