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

root = G

sn1P1 = Tr808.sn' $ TrSpec 0.8 0 342 (Just 0.085)
sn1P2 = Tr808.sn' $ TrSpec 0.8 3 300 (Just 0.085)
sn1P3 = Tr808.sn' $ TrSpec 0.8 6 259 (Just 0.085)
sn1P4 = Tr808.sn' $ TrSpec 0.8 9 200 (Just 0.085)

song :: SongM
song = do
  numBeats <- asks (view beatLength)
  gBPM <- asks (view bpm)

  kcks1 <- drums "X _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|_ _ _ _ O _ _ _|_ _ _ _ _ _ _ _|" Hm.bd2
  kcks2 <- drums "X _ _ _ O _ _ _|_ _ _ _ _ _ _ _|_ _ _ _ O _ _ _|_ _ _ _ _ _ _ _|" Hm.bd2
  kcks3 <- drums "X _ _ _ _ _ _ _|o _ _ _ _ _ _ _|_ _ _ _ X _ _ _|o _ _ _ _ _ _ _|" Hm.bd2
  kcks4 <- drums "O _ _ _ o _ _ _|_ _ _ _ _ _ _ _|_ _ _ _ O _ _ _|_ _ _ _ _ _ _ _|" Tr808.bd2
  kcks5 <- drums "X _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|_ _ _ _ O _ _ _|_ _ _ _ _ _ _ _|" Tr808.bd
  kcks6 <- drums "X _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|_ _ _ _ O _ _ _|_ _ _ _ _ _ _ _|" Hm.bd1
  kcks7 <- drums "O _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|_ _ _ _ o _ _ _|_ _ _ _ _ _ _ _|" Tr808.bd2

  snrs1 <- drums "_ _ _ _ _ _ _ _|X _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|X _ _ _ _ _ _ _|" Hm.sn1
  snrs2 <- drums "_ _ _ _ _ _ _ _|X _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|" Hm.sn1
  snrs3 <- drums "_ _ _ _ _ _ _ _|X _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|X _ _ _ _ _ _ _|" Tr808.sn

  snrsP1 <- drums "_ _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|X _ _ _ _ _ _ _|" sn1P4
  snrsP2 <- drums "_ _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|_ _ X _ _ _ _ _|" sn1P3
  snrsP3 <- drums "_ _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|_ _ _ _ X _ _ _|" sn1P2
  snrsP4 <- drums "_ _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|_ _ _ _ _ _ X _|" sn1P1
  let psnrs = har [snrsP1, snrsP2, snrsP3, snrsP4]

  chhs1 <- drums ". _ _ _ . _ _ _|_ _ _ _ . _ _ _|. _ _ _ _ _ _ _|_ _ _ _ . _ _ _|" Hm.chh
  chhs2 <- drums "O _ _ _ . _ _ _|. _ _ _ . _ _ _|o _ _ _ . _ _ _|. _ _ _ . _ _ _|" Hm.chh
  chhs3 <- drums "_ _ _ _ _ _ _ _|_ _ _ _ _ _ . _|_ _ . _ _ _ _ _|_ _ _ _ _ _ . _|" Tr808.chh

  ohhs1 <- drums "_ _ _ _ _ _ _ _|_ _ _ _ _ _ . _|_ _ . _ _ _ _ _|_ _ _ _ _ _ . _|" Hm.ohh
  ohhs2 <- drums "_ _ _ _ _ _ _ _|_ _ _ _ _ _ . _|_ _ . _ _ _ _ _|_ _ _ _ _ _ . _|" Tr808.ohh
  ohhs3 <- drums ". _ _ _ . _ _ _|_ _ _ _ . _ _ _|. _ _ _ _ _ _ _|_ _ _ _ . _ _ _|" Tr808.ohh

  cyms1 <- drums "O _ _ _ _ _ _ _|O _ _ _ _ _ _ _|o _ _ _ _ _ _ _|O _ _ _ _ _ _ _|" Tr808.cym

  clps1 <- drums "_ _ _ _ _ _ _ _|X _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|X _ _ _ _ _ _ _|" Hm.clap

  pad <-
    compileI nightPad
    $ repeatToBeats numBeats
    $ Pch <$> minorChord root ?? 6 ?? 0.5 ?? 16

  bass <-
    compileI (withDeepBass 1.0 pwBass)
    $ repeatToBeats numBeats
    $ Pch <$> [root, doN 2 succC root, doN 3 succC root, doN 7 succC root] <*> [5] <*> [0.5, 0.0] <*> [2]

  lead <-
    compileI frenchHorn
    $ repeatToBeats numBeats
    $ ((expandScale [6, 7, 8] (minorScale root) !!) <$> [0, 5, 5, 2, 14, 20, 12, 12, 3, 2, 7, 7]) <*> [0.4] <*> replicate 4 0.25

  let patterns =
        fmap (rever2 0.2) . har
        <$> [ [cyms1]
            , [kcks4, snrs3, chhs3, ohhs3, cyms1, clps1]
            , [cyms1]
            , [kcks4, snrs3, chhs3, ohhs3, cyms1, clps1]
            , [cyms1]
            ]

  allDrums <- cotraverse mel (forBeats 32 <$> patterns)
  return $ har [ allDrums
               , bass
               --, lead
               , restSig (Beats gBPM 28) +:+ lead
               ]

songEnv = SongEnv { _bpm=174
                  , _beatLength=128
                  }
dnb' = runSongM songEnv song
dnb = dac =<< dnb'
