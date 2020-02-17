{-# LANGUAGE FlexibleContexts #-}

module TechnoPlayground where

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

root = D

song :: SongM
song = do
  kcks1 <- drums "O _ _ _ _ _ _ _|o _ _ _ _ _ _ _|O _ _ _ _ _ _ _|o _ _ _ _ _ _ _|" Tr808.bd2
  snrs1 <- drums "_ _ _ _ _ _ _ _|X _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|X _ _ _ _ _ _ _|" Tr808.sn
  chhs1 <- drums ". _ _ _ _ _ _ _|o _ _ _ _ _ _ _|. _ _ _ _ _ _ _|. _ _ _ _ _ _ _|" Tr808.chh
  ohhs1 <- drums ". _ _ _ . _ _ _|_ _ _ _ . _ _ _|. _ _ _ _ _ _ _|_ _ _ _ . _ _ _|" Tr808.ohh
  cyms1 <- drums "O _ o _ . _ . _|" Tr808.cym
  clps1 <- drums "_ _ _ _ _ _ _ _|X _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|X _ _ _ _ _ _ _|" Hm.clap

  let pat0 = har [kcks1]
      pat1 = har [kcks1, clps1]
      pat2 = har [kcks1, clps1, cyms1]

  pad <-
    compileI dreamPad
    [ Pch root 6 0.8 8
    , Silent 8
    ]

  looped <-
    cotraverse mel [ forBeats 16 pat0
                   , forBeats 16 pat1
                   , forBeats 32 pat2
                   ]

  return $ loop looped =:= loop pad

songEnv = SongEnv { _bpm=140
                  , _beatLength=512
                  }
tec' = runSongM songEnv song
tec = dac =<< tec'
