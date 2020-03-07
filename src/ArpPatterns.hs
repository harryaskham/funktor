module ArpPatterns where

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

type ArpDef = [Note] -> Octave -> Velocity -> Duration -> [Pch]

arpPat1 :: ArpDef
arpPat1 notes octave velocity duration =
  getZipList
  $ Pch
  <$> ZipList ((cycle notes !!) <$> [0, 1, 2, 0, 2, 2, 3, 3])
  <*> ZipList (replicate 4 octave ++ replicate 4 (octave-1))
  <*> ZipList (repeat velocity)
  <*> ZipList (repeat duration)

arpPat2 :: ArpDef
arpPat2 notes octave velocity duration =
  getZipList
  $ Pch
  <$> ZipList ((cycle notes !!) <$> [7, 1, 3, 6, 1, 3])
  <*> ZipList (replicate 3 octave ++ replicate 3 (octave-1))
  <*> ZipList (repeat velocity)
  <*> ZipList (repeat duration)

song = do
  d <- do
    b <- drums "X|." Tr808.bd2
    c <- drums "O _ o |o . .|" Tr808.chh
    cl <- drums "_|o" Hm.clap
    return $ har [b, c, cl]

  a1 <- do
    i <- compileI banyan $ arpPat1 (majorChord Eb) 8 0.5 2
    e <- sqrTabEnv [OnFor 8, OffFor 4]
    return $ stereoMap (*e) <$> i

  a2 <- do
    i <- compileI simpleBass $ arpPat2 (minorScale C) 8 0.5 1
    e <- sqrTabEnv [OffFor 4, OnFor 8]
    return $ stereoMap (*e) <$> i

  a3 <- do
    i <- compileI epiano2 $ arpPat2 (minorScale C) 7 0.5 4
    e <- sqrTabEnv [OnFor 12, OffFor 4]
    return $ stereoMap (*e) <$> i

  a4 <- do
    i <- compileI fmBass1 $ arpPat1 (majorChord Bb) 5 0.5 4
    e <- sqrTabEnv [OnFor 6, OffFor 3]
    return $ stereoMap (*e) <$> i

  return $ har [d, a1, a2, a3, a4]


songEnv = SongEnv { _bpm=140
                  , _beatLength=1024
                  }

tst' = runSongM songEnv song
tst = dac =<< tst'
