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
  gBPM <- asks (view bpm)

  pad' <-
    compileI razorPad
    [ Pch root 8 0.4 8
    , Pch (doN 7 succC root) 8 0.4 4
    , Pch (doN 3 succC root) 8 0.4 4
    ]
  let pad = stereoMap (sqrEnv gBPM 0 (1/8) *) <$> pad'

  lead1' <-
    compileI sawOrgan
    $ takeIxs [3, 9, 1, 8]
    $ sort
    $ expandScale [7, 8, 9] (minorScale root) ?? 0.2 ?? (1/2)
  let lead1 = stereoMap (sqrEnv gBPM 0.75 32 *) <$> lead1'

  high' <- compileI dreamPad [ Pch root 9 0.6 16 ]
  let high = stereoMap ((sinEnv gBPM 0 4 * sqrEnv gBPM 0 (1/8)) *) <$> high'

  lead2' <-
    compileI triOrgan
    $ takeIxs [12, 12, 11, 13, 13, 14, 10, 15]
    $ sort
    $ expandScale [8, 9, 10] (minorScale root) ?? 0.3 ?? (1/2)
  let lead2 = stereoMap (sqrEnv gBPM 0.25 16 *) <$> lead2'

  lead3' <-
    compileI sqrOrgan
    $ takeIxs [1, 2, 3, 4, 3, 4, 3, 4]
    $ sort
    $ expandScale [8, 9, 10] (minorScale root) ?? 0.2 ?? (1/2)
  let lead3 = stereoMap (sqrEnv gBPM 0.5 16 *) <$> lead3'

  lead4' <-
    compileI sawOrgan
    $ takeIxs [10, 9, 8, 6, 3, 6, 3, 6]
    $ sort
    $ expandScale [8, 9, 10] (minorScale root) ?? 0.2 ?? (1/2)
  let lead4 = stereoMap (sqrEnv gBPM 0 16 *) <$> lead4'

  kcks <- drums "X _ _ _|o _ _ _|o _ _ _|o _ _ _|" Tr808.bd2
  chhs <- drums "O . _ .|_ . _ .|. . _ .|_ . _ .|" Tr808.chh
  ohhs <- drums "_ _ o _|_ _ . _|_ _ . _|_ _ . _|" Tr808.ohh
  clps <- drums "_ _ _ _|X _ _ _|_ X _ _|X _ _ _|" Hm.clap
  clls <- drums "X X X X|" Tr808.cl

  return $ har [kcks, clls, pad, clps, high, chhs, ohhs, lead1, lead2, lead3, lead4]

songEnv = SongEnv { _bpm=128
                  , _beatLength=8*64
                  }

gar' = runSongM songEnv song
gar = dac =<< gar'
