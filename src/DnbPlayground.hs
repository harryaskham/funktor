{-# LANGUAGE FlexibleContexts #-}

module DnbPlayground where

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
import Tabs
import Tools

-- TODO:
-- named patterns and arps that we can concat and introduce expressively
-- Pitch drums and make actual genuine drum n bass.
-- Lots of different drum sections that can reuse whenever
-- fade in and out with envelopes, make variants of each type of pattern, and put down the beats I know
-- can then do like 16 repeats with a drop suffix of a break- make a library of breaks
-- play light pad over, etc etc etc- vocal samples - just need to pitch the drums
-- drops / fills / breaks

root = D

sn1P1 = Tr808.sn1' $ TrSpec 0.8 0 342 (Just 0.085)

sn1P2 = Tr808.sn1' $ TrSpec 0.8 3 300 (Just 0.085)

sn1P3 = Tr808.sn1' $ TrSpec 0.8 6 259 (Just 0.085)

sn1P4 = Tr808.sn1' $ TrSpec 0.8 9 200 (Just 0.085)

song :: SongM
song = do
  kcks1 <- drums "X _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|_ _ _ _ O _ _ _|_ _ _ _ _ _ _ _|" Hm.bd2
  kcks2 <- drums "X _ _ _ O _ _ _|_ _ _ _ _ _ _ _|_ _ _ _ O _ _ _|_ _ _ _ _ _ _ _|" Hm.bd2
  kcks3 <- drums "X _ _ _ _ _ _ _|o _ _ _ _ _ _ _|_ _ _ _ X _ _ _|o _ _ _ _ _ _ _|" Hm.bd2
  kcks4 <- drums "O _ _ _ o _ _ _|_ _ _ _ _ _ _ _|_ _ _ _ O _ _ _|_ _ _ _ _ _ _ _|" Tr808.bd2
  kcks5 <- drums "X _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|_ _ _ _ O _ _ _|_ _ _ _ _ _ _ _|" Tr808.bd
  kcks6 <- drums "X _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|_ _ _ _ O _ _ _|_ _ _ _ _ _ _ _|" Hm.bd1
  kcks7 <- drums "O _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|_ _ _ _ o _ _ _|_ _ _ _ _ _ _ _|" Tr808.bd2

  snrs1 <- drums "_ _ _ _ _ _ _ _|X _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|X _ _ _ _ _ _ _|" Hm.sn1
  snrs2 <- drums "_ _ _ _ _ _ _ _|X _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|" Hm.sn1
  snrs3 <- drums "_ _ _ _ _ _ _ _|X _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|X _ _ _ _ _ _ _|" Tr808.sn1

  snrsP1 <- drums "_ _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|X _ _ _ _ _ _ _|" sn1P4
  snrsP2 <- drums "_ _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|_ _ X _ _ _ _ _|" sn1P3
  snrsP3 <- drums "_ _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|_ _ _ _ X _ _ _|" sn1P2
  snrsP4 <- drums "_ _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|_ _ _ _ _ _ X _|" sn1P1
  let psnrs = har [snrsP1, snrsP2, snrsP3, snrsP4]

  chhs1 <- drums ". _ _ _ . _ _ _|_ _ _ _ . _ _ _|. _ _ _ _ _ _ _|_ _ _ _ . _ _ _|" Hm.chh
  chhs2 <- drums "O _ _ _ . _ _ _|. _ _ _ o _ _ _|o _ _ _ . _ _ _|. _ _ _ . _ _ _|" Hm.chh
  chhs3 <- drums "_ _ _ _ _ _ _ _|_ _ _ _ _ _ . _|_ _ . _ _ _ _ _|_ _ _ _ _ _ . _|" Tr808.chh
  chhs4 <- drums "O _ _ _ . _ _ _|. _ _ _ o _ _ _|o _ _ _ . _ _ _|. _ _ _ . _ _ _|" Tr808.chh

  ohhs1 <- drums "_ _ _ _ _ _ _ _|_ _ _ _ _ _ . _|_ _ . _ _ _ _ _|_ _ _ _ _ _ . _|" Hm.ohh
  ohhs2 <- drums "_ _ _ _ _ _ _ _|_ _ _ _ _ _ . _|_ _ . _ _ _ _ _|_ _ _ _ _ _ . _|" Tr808.ohh
  ohhs3 <- drums ". _ _ _ . _ _ _|_ _ _ _ . _ _ _|. _ _ _ _ _ _ _|_ _ _ _ . _ _ _|" Tr808.ohh

  cyms1 <- drums "O _ _ _ _ _ _ _|O _ _ _ _ _ _ _|o _ _ _ _ _ _ _|O _ _ _ _ _ _ _|" Tr808.cym

  clps1 <- drums "_ _ _ _ _ _ _ _|X _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|X _ _ _ _ _ _ _|" Hm.clap

  break1 <-
    cotraverse
      har
      [ drums "O _ _ _|o _ _ _|o _ _ _|o _ _ _|O _ o _|o _ o _|. . o o|O O X X|" Tr808.cym
      --drums "o . _ _|. _ . _|_ _ . _|. _ . .|_ _ o _|. _ . _|. . . .|_ . _ .|" Tr808.chh
      --, drums "_ _ O _|_ . _ _|X _ _ .|_ _ _ _|X _ _ O|_ . _ _|O o . .|_ _ o _|" Tr808.sn
      ]

  intro <-
    cotraverse
      mel
      [ forBeats 24 (har [kcks7, chhs4, snrs3]),
        forBeats 8 break1
      ]

  let pat0 = har [kcks7, snrs3, chhs4, cyms1]
      pat1 = har [kcks7, snrs3, chhs4, cyms1, clps1]

  pad <-
    compileI dreamPad $
      Pch <$> minorChord root ?? 6 ?? 0.5 ?? 16

  bass <-
    compileI (withDeepBass 1.0 pwBass) $
      Pch
        <$> [root, doN 2 succC root, doN 3 succC root, doN 7 succC root]
        <*> [5]
        <*> [0.5, 0.0]
        <*> [2]

  lead <-
    compileI frenchHorn $
      ( (expandScale [6, 7, 8] (minorScale root) !!)
          <$> [0, 5, 5, 2, 14, 20, 12, 12, 3, 2, 7, 7]
      )
        <*> [0.4]
        <*> replicate 4 0.25

  arp1 <-
    compileI epiano2 $
      ( (expandScale [6, 7, 8] (minorScale root) !!)
          <$> [0, 1, 2, 3, 10, 9, 8, 7, 2, 3, 4, 5, 16, 15, 14, 13]
      )
        <*> [0.4]
        <*> [1]

  let arpat1 root =
        take 8 . cycle $ Pch <$> minorChord root ++ majorChord (doN 5 succC root) <*> [7] <*> [0.5] <*> [1 / 2]
      arpat2 root =
        doN <$> [0, 2, 0, 3, 0, 5, 0, 6] <*> [succN] <*> [Pch root 7 0.5 (1 / 2)]
      arpat3 root =
        doN <$> [0, 2, 3, 2, 3, 5, 7, 5, 7, 8, 10, 8, 10, 12, 14, 15] <*> [succN] <*> [Pch root 7 0.3 (1 / 2)]
      arpat4 root =
        doN <$> [0, 0, 0, -1, 3, 3, 3, 2] <*> [succN] <*> [Pch root 7 0.3 (1 / 2)]
      arpat5 root =
        getZipList
          ( ZipList (Pch <$> (doN <$> [0, 7, 6, 5, 3, 5, 3, 2] <*> [succC] <*> [root]) <*> [6] <*> [0.5])
              <*> ZipList [5, 1, 1, 1, 5, 1, 1, 1]
          )
      arpat6 root = (sort (expandScale [7, 8, 9] (minorChord root) <*> [0.3] <*> [1 / 2]) !!) <$> randomRs (0, 8) (mkStdGen 42)
      arpat7 root = (sort (expandScale [7, 8] (majorScale root) <*> [0.2] <*> [1 / 2]) !!) <$> randomRs (0, 13) (mkStdGen 666)
      arpat8 root = (sort (expandScale [7, 8, 9] (majorScale root) <*> [0.1] <*> [2]) !!) <$> randomRs (0, 13) (mkStdGen 66)
      arpat9 root =
        getZipList $
          (ZipList $ Pch <$> cycle (reverse $ majorChord root) <*> [6] <*> [0.5])
            <*> ZipList [5, 3, 5, 3]

  arp2 <- compileI guitar $ arpat1 root
  arp3 <- compileI banyan $ arpat2 root
  arp4 <- compileI accordeon $ arpat3 root
  arp5 <- compileI epiano2 $ arpat4 root
  arp6 <- compileI brokenAccordeon $ arpat4 (doN 5 succC root)
  arp7 <- cotraverse (loop . mel) [forBeats 8 arp5, forBeats 8 arp6]
  arp8 <- compileI mutedBassClarinet $ arpat5 root
  arp9 <- compileI epiano1 $ arpat6 root
  arp10 <- compileI epiano1 $ take 32 $ arpat7 root
  arp11 <- compileI epiano1 $ take 32 $ arpat8 root
  arp12 <- compileI (withDeepBass 1.0 simpleBass) $ arpat9 root

  return $ pat1 =:= arp12 =:= arp10 =:= arp11

songEnv =
  SongEnv
    { _bpm = 174,
      _beatLength = 512
    }

dnb' = runSongM songEnv song

dnb = dac =<< dnb'
