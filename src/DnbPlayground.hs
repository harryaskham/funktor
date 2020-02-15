{-# LANGUAGE FlexibleContexts #-}

module DnbPlayground where

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

-- TODO:
-- pitched drums, with FX, movd out to own module so that we can use across songs
-- named patterns and arps that we can concat and introduce expressively
-- Pitch drums and make actual genuine drum n bass.
-- Lots of different drum sections that can reuse whenever
-- fade in and out with envelopes, make variants of each type of pattern, and put down the beats I know
-- can then do like 16 repeats with a drop suffix of a break- make a library of breaks
-- play light pad over, etc etc etc- vocal samples - just need to pitch the drums
-- drops / fills / breaks
-- should be able to pitch down samples since Sam Sig2 is a functor
-- Need to solve the 

root = C

sn1P1 = Hm.sn1
sn1P2 = stereoMap (scalePitch 3) <$> Hm.sn1
sn1P3 = stereoMap (scalePitch 6) <$> Hm.sn1
sn1P4 = stereoMap (scalePitch 9) <$> Hm.sn1

song :: SongM
song = do
  numBeats <- asks (view beatLength)

  kcks1 <- drums "X _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|_ _ _ _ O _ _ _|_ _ _ _ _ _ _ _|" Hm.bd2
  kcks2 <- drums "X _ _ _ O _ _ _|_ _ _ _ _ _ _ _|_ _ _ _ O _ _ _|_ _ _ _ _ _ _ _|" Hm.bd2

  snrs1 <- drums "_ _ _ _ _ _ _ _|X _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|X _ _ _ _ _ _ _|" Hm.sn1
  snrs2 <- drums "_ _ _ _ _ _ _ _|X _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|" Hm.sn1

  snrsP1 <- drums "_ _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|X _ _ _ _ _ _ _|" sn1P1
  snrsP2 <- drums "_ _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|_ _ X _ _ _ _ _|" sn1P2
  snrsP3 <- drums "_ _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|_ _ _ _ X _ _ _|" sn1P3
  snrsP4 <- drums "_ _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|_ _ _ _ _ _ X _|" sn1P4

  chhs1 <- drums ". _ _ _ . _ _ _|_ _ _ _ . _ _ _|. _ _ _ _ _ _ _|_ _ _ _ . _ _ _|" Hm.chh
  chhs2 <- drums "O _ _ _ . _ _ _|. _ _ _ . _ _ _|o _ _ _ . _ _ _|. _ _ _ . _ _ _|" Hm.chh

  ohhs1 <- drums "_ _ _ _ _ _ _ _|_ _ _ _ _ _ . _|_ _ . _ _ _ _ _|_ _ _ _ _ _ . _|" Hm.ohh

  clps1 <- drums "_ _ _ _ _ _ _ _|X _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|X _ _ _ _ _ _ _|" Hm.clap

  -- Put together a few different dnb patterns
  let patterns =
        har
        <$> [ pure snrsP1
            , pure snrsP2
            , pure snrsP3
            , pure snrsP4
            , [kcks1, snrs1]
            , [snrsP1, snrsP2, snrsP3, snrsP4]
            , [kcks2, snrs2, snrsP1, snrsP2, snrsP3, snrsP4]
            , [kcks1, snrs1, chhs1]
            , [kcks1, snrs1, chhs1, ohhs1]
            , [kcks1, snrs1, chhs1, ohhs1, clps1]
            ]

  -- Sample two bars of each pattern
  cotraverse mel (forBeats 8 <$> patterns)

songEnv = SongEnv { _bpm=174
                  , _beatLength=128
                  }
dnb' = runSongM songEnv song
dnb = dac =<< dnb'
