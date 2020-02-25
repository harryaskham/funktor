{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

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
  pad <-
    compileI dreamPad
    [ Pch root 8 0.6 8
    , Pch (doN 5 succC root) 8 0.6 4
    , Pch (doN 3 succC root) 8 0.6 4
    ]
  gBPM <- asks (view bpm)
  return $ stereoMap (sqrEnv gBPM 0 (1/8) *) <$> pad

songEnv = SongEnv { _bpm=128
                  , _beatLength=128
                  }

gar' = runSongM songEnv song
gar = dac =<< gar'
