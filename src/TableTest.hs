{-# LANGUAGE FlexibleContexts #-}

module TableTest where

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

root = E

song :: SongM
song = do
  gBPM <- asks (view bpm)

  test' <-
    compileI sawOrgan
    [ Pch root 8 0.5 1
    , Silent 1
    ]
  let test = stereoMap (sqrEnv gBPM 0 4 *) <$> test'

  return test
  
songEnv = SongEnv { _bpm=140
                  , _beatLength=1024
                  }

tt' = runSongM songEnv song
tt = dac =<< tt'
