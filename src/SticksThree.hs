{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module SticksThree where

import Csound.Base hiding (Tab, clp, Duration)
import Csound.Sam
import Csound.Patch
import Tabs
import Tools
import Note
import Melody
import Sample
import Series
import Fugue
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
  gtr <- do
    e <- sqrEnvM 0 32
    i <- loop . mul 0.15 <$> loadWav 4 1.02 "samples/Tapping.wav"
    return $ mul e <$> i

  kcks <- drums "X _ _ _|O _ _ _|O _ _ _|O _ _ _" Tr808.bd2
  chhs <- drums "O _ o _|. _ . _|" Tr808.chh
  ohhs <- drums "_ _ _ .|_ _ _ .|" Tr808.ohh
  drumEnvs <-
    sequence
    $ getZipList
    $ sqrEnvM
    <$> ZipList [0.0, 0.0, 0.5]
    <*> ZipList [16, 16, 32]
  let drms = mul 0.5 <$> har $ zipEnvsWith (*) drumEnvs [ohhs, chhs, kcks]

  voiceEnvs <-
    sequence
    $ sqrEnvM
    <$+> [0, 0.5, 0.3, 0.4, 0.6, 0.8, 0.9]
    <*++> [16, 16, 32, 32, 64, 64]

  let fugue = Fugue { _theme=[E, C, A, E, G]
                    , _octaves=[8, 8, 8, 8, 8]
                    , _voices=[ \ns os -> Pch <$+> ns <*+> (subtract 1 <$> os) <*+> repeat 0.25 <*++> repeat (2/3)
                              , \ns os -> Pch <$+> ns <*+> os <*+> repeat 0.25 <*++> cycle [1, 2]
                              , \ns os -> Pch <$+> ns <*+> (subtract 1 <$> os) <*+> repeat 0.25 <*++> cycle [4, 8]
                              , \ns os -> Pch <$+> reverse ns <*+> ((+1) <$> os) <*+> repeat 0.25 <*++> cycle [6/4, 1/2, 1, 1]
                              , \ns os -> Pch <$+> ns <*+> ((+1) <$> os) <*+> repeat 0.25 <*++> repeat 4
                              , \ns os -> Pch <$+> reverse ns <*+> os <*+> repeat 0.25 <*++> repeat (5/2)
                              , \ns os -> Pch <$+> ns <*+> os <*+> repeat 0.25 <*++> repeat (1/2)
                              ]
                    , _voiceEnvs=voiceEnvs
                    , _instruments=repeat guitar
                    -- , _instruments=[sqrOrgan, sawOrgan, hammondOrgan, triOrgan, epiano2, banyan, harpsichord]
                    , _enterBeats=8
                    }
  fC <- compileFugue fugue

  return $ har [drms, smallRoom2 <$> fC, gtr]

songEnv = SongEnv { _bpm=128
                  , _beatLength=256
                  }

st3' = runSongM songEnv song
st3 = dac =<< st3'
