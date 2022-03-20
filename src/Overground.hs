{-# LANGUAGE FlexibleContexts #-}

module Overground where

import Control.Lens
import Control.Monad.Random
import Control.Monad.Reader
import Csound.Base hiding (Duration, Tab, clp)
import Csound.Catalog.Drum.Hm as Hm
import Csound.Catalog.Drum.Tr808 as Tr808
import Csound.Catalog.Effect
import Csound.Patch
import Csound.Sam
import Data.List
import Data.Ord
import Data.Sort
import Melody
import Note
import System.IO.Unsafe
import System.Random
import Tabs
import Tools

root = Ab

song :: SongM
song = do
  a <-
    compileI overtoneLead $
      Pch <$> minorChord Ab ?? 7 ?? 0.5 ?? 1
  b <-
    compileI banyan $
      Pch <$> minorScale Ab ?? 8 ?? 0.5 ?? (6 / 4)
  c <- drums "X" Tr808.bd2
  d <- drums "X o o o" Tr808.chh
  e <- drums "O _ O _" Tr808.cl
  f <- drums "_ _ X _" Tr808.sn1
  en1 <- sqrEnvM 8 0
  en2 <- sqrEnvM 8 0
  en3 <- sqrEnvM 8 0
  en4 <- sqrEnvM 8 0
  en5 <- sqrEnvM 8 0
  en6 <- sqrEnvM 8 0

  return $ har [a & withEnv en1, b, c, d, e, f]

songEnv =
  SongEnv
    { _bpm = 128,
      _beatLength = 1024
    }

ovg' = runSongM songEnv song

ovg = dac =<< ovg'
