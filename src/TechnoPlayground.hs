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

-- TODO here:
-- randomised section generator that spits out
-- length, arptype, envelope mix, effect mix
--
-- For later: slow right down, introduce noise, and make
-- binaural / infinite ambient generator

root = D

song :: SongM
song = do
  kcks1 <- drums "O _ _ _ _ _ _ _|o _ _ _ _ _ _ _|O _ _ _ _ _ _ _|o _ _ _ _ _ _ _|" Tr808.bd2
  snrs1 <- drums "_ _ _ _ _ _ _ _|X _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|X _ _ _ _ _ _ _|" Tr808.sn
  chhs1 <- drums "_ _ _ _ o _ _ _|_ _ _ _ . _ _ _|_ _ _ _ o _ _ _|_ _ _ _ . _ _ _|" Tr808.chh
  ohhs1 <- drums "o _ _ _ o _ _ _|" Tr808.ohh
  cyms1 <- drums "O _ o _ . _ . _|" Tr808.cym
  clps1 <- drums "_ _ _ _ _ _ _ _|X _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|X _ _ _ _ _ _ _|" Hm.clap

  let pat0 = har [kcks1]
      pat1 = har [kcks1, clps1]
      pat2 = har [kcks1, clps1, chhs1]
      pat3 = har [kcks1, clps1, ohhs1]
      pat4 = har [kcks1, clps1, cyms1]

  pad <-
    -- TODO: rename to e.g. instr
    compileI dreamPad
    [ Pch root 6 0.8 8
    , Silent 8
    ]

  bass <-
    compileI fmBass2
    [ Pch root 6 0.8 (1/2)
    , Pch (doN 3 succC root) 6 0.8 (1/2)
    , Pch (predC root) 6 0.8 (1/2)
    ]

  drums <-
    cotraverse mel [ forBeats 16 pat0
                   , forBeats 16 pat1
                   , forBeats 32 pat2
                   , forBeats 32 pat3
                   , forBeats 32 pat4
                   ]

  return $ loop drums =:= loop pad =:= bass

songEnv = SongEnv { _bpm=140
                  , _beatLength=1024
                  }
tec' = runSongM songEnv song
tec = dac =<< tec'
