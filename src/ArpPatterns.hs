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

arpPat3 :: ArpDef
arpPat3 notes octave velocity duration =
  getZipList
  $ Pch
  <$> ZipList ((cycle notes !!) <$> [3, 3, 1, 2, 1])
  <*> ZipList (repeat octave)
  <*> ZipList (repeat velocity)
  <*> ZipList (repeat duration)

song = do
  d <- do
    b <- drums "X|.|" Tr808.bd2
    c <- drums "_ . . .|" Tr808.chh
    o <- drums ". _ _ _|" Tr808.ohh
    cl <- drums "_|o|" Hm.clap
    return $ har [b, c, o, cl]

  a1 <- do
    i <- compileI banyan $ arpPat1 (majorChord Eb) 8 0.3 2
    e <- sinEnvM 0.8 (bars 6)
    return $ stereoMap (*e) <$> i

  a2 <- do
    i <- compileI simpleBass $ arpPat2 (minorScale C) 8 0.3 1
    e <- sinEnvM 0.4 (bars 4)
    return $ stereoMap (*e) <$> i

  a3 <- do
    i <- compileI epiano2 $ arpPat2 (minorScale C) 7 0.2 4
    e <- sinEnvM 0.5 (bars 3)
    return $ stereoMap (*e) <$> i

  a4 <- do
    i <- compileI fmBass1 $ arpPat1 (majorChord Bb) 5 0.3 4
    e <- sinEnvM 0.3 (bars 7)
    return $ stereoMap (*e) <$> i

  a5 <- do
    i <- compileI fmDroneFastm $ arpPat3 (minorChord C) 8 0.3 8
    e <- sinEnvM 0.5 (bars 2)
    return $ stereoMap (*e) <$> i

  a6 <- do
    i <- compileI razorLead $ arpPat3 (majorScale C) 8 0.5 (1/2)
    e <- sinEnvM 0 (bars 3)
    return $ stereoMap (*e) <$> i

  return $ har [d, a1, a2, a3, a4, a5, a6]


songEnv = SongEnv { _bpm=140
                  , _beatLength=1024
                  }

tst' = runSongM songEnv song
tst = dac =<< tst'
