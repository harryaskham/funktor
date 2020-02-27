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

  kcks <- drums "X _ O _|" Tr808.bd2
  clls <- drums ". . . .|" Tr808.cl

  test <- do
    i <- compileI sqrOrgan [ Pch root 8 0.5 (1/4)
                           , Silent (1/4)
                           ]
    e <- sqrTabEnv [OnFor 2, OffFor 6]
    return $ stereoMap (e*) <$> i
 
  return $ har [test, kcks, clls]
  
songEnv = SongEnv { _bpm=170
                  , _beatLength=1024
                  }

tt' = runSongM songEnv song
tt = dac =<< tt'
