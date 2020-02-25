{-# LANGUAGE FlexibleContexts #-}

module Garage where

import Csound.Base hiding (Tab, clp, Duration)
import Csound.Sam
import Csound.Patch
import Tabs
import Tools
import Note
import Melody
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

root = D

song :: SongM
song = do
  pad' <-
    compileI nightPad
    [ Pch root 8 0.6 8
    , Pch (doN 5 succC root) 8 0.6 4
    , Pch (doN 3 succC root) 8 0.6 4
    ]
  gBPM <- asks (view bpm)
  let pad = stereoMap (sqrEnv gBPM 0 (1/8) *) <$> pad'

  kcks <- drums "X _ _ _|o _ _ _|o _ _ _|o _ _ _|" Tr808.bd2
  chhs <- drums "_ _ _ _|_ . _ .|. . _ .|_ . _ .|" Tr808.chh
  ohhs <- drums "_ _ _ _|_ _ . _|_ _ . _|_ _ . _|" Tr808.ohh
  snrs <- drums "_ _ _ _|X _ _ _|_ _ _ _|X _ _ _|" Tr808.sn
  clls <- drums "X X X X|" Tr808.cl

  return $ har [pad, kcks, chhs, ohhs, snrs, clls]

songEnv = SongEnv { _bpm=128
                  , _beatLength=128
                  }

gar' = runSongM songEnv song
gar = dac =<< gar'
