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

-- TODO: drop imports
-- one theme
-- simple syntax
-- use squares to bring voices in
-- instr list in order
-- theme in different orders
-- sometimes reversed! crab
-- have voices oscillate after theme introduction - fuguewave, const then fugue - need a max-of wave combiner
-- then either just repeat theme, or improvise on theme, or improvise in key
-- find instruments that work
-- guitar becomes just one instrument
-- drums also oscillate
-- oscillate with primes and calculate cycle, truncate at perfect loop

song :: SongM
song = do
  kcks <- drums "X _ _ _|O _ _ _|O _ _ _|O _ _ _" Tr808.bd2
  guitar <- loop <$> loadWav 16 0.9 "samples/Echoplex.wav"

  return $ har [kcks, guitar]

songEnv = SongEnv { _bpm=128
                  , _beatLength=128
                  }

st3' = runSongM songEnv song
st3 = dac =<< st3'
