{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module SticksThree where

import Csound.Base hiding (Tab, clp, Duration)
import Csound.Sam
import Csound.Patch
import Tabs
import Tools
import Note
import Melody
import Sample
import Series
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

data Fugue = Fugue { _theme :: [Note]
                   , _octaves :: [Octave]
                   , _voices :: [[Note] -> [Octave] -> [Pch]]
                   , _instruments :: [Patch2]
                   , _enterBeats :: Int
                   }
makeLenses ''Fugue

zipEnvsWith :: (SegEnv -> Sig -> Sig) -> [SegEnv] -> [Seg Sig2] -> [Seg Sig2]
zipEnvsWith f es ss = getZipList $ (fmap <$> (stereoMap <$> (f <$> ZipList es))) <*> ZipList ss

compileFugue :: (MonadReader SongEnv m, MonadRandom m) => Fugue -> m (Seg Sig2)
compileFugue f = do
  -- A set of envelopes that have increasingly long intro times
  introEnvs <- traverse (\i -> sqrTabEnv [OffFor (fromIntegral $ i*f^.enterBeats), OnFor 1024]) [0..20]
  let continuation = expandScale [7, 8, 9] (f^.theme) <*> [0.25, 0.2, 0.1] <*> [4, 8, 3/2, 1, 2, 10/4, 15/4]
  continuationIxs <- replicateM (length (f^.voices)) $ getRandomRs (0, length continuation - 1)
  let continuations = takeIxs <$> continuationIxs ?? continuation
      themeNotes = repeatToBeats (length (f^.voices) * f^.enterBeats) <$> ((f^.voices) ?? (f^.theme) ?? (f^.octaves))
      -- notesPerVoice = (++) <$> themeNotes <*> continuations
      notesPerVoice = themeNotes
  compiledVoices <-
    sequence
    $ getZipList
    $ ZipList (compileI <$> f^.instruments)
    <*> ZipList notesPerVoice
  return $ har $ zipEnvsWith (\e1 e2 -> minabs [e1,e2]) introEnvs compiledVoices

song :: SongM
song = do
  let fugue = Fugue { _theme=[D, F, Bb, A, D, C]
                    , _octaves=[8, 8, 8, 8, 8, 8]
                    , _voices=[ \ns os -> Pch <$> ns <*> (subtract 2 <$> os) ?? 0.25 ?? 1
                              , \ns os -> Pch <$> ns <*> os ?? 0.25 ?? 1
                              , \ns os -> Pch <$> ns <*> (subtract 1 <$> os) ?? 0.25 ?? 8
                              , \ns os -> Pch <$> reverse ns <*> ((+1) <$> os) ?? 0.25 ?? (5/4)
                              , \ns os -> Pch <$> ns <*> ((+1) <$> os) ?? 0.25 ?? 16
                              , \ns os -> Pch <$> reverse ns <*> os ?? 0.25 ?? (3/2)
                              , \ns os -> Pch <$> ns <*> os ?? 0.25 ?? (1/3)
                              ]
                    -- , _instruments=[fmBass1, epiano2, hammondOrgan, banyan, overtonePad, dreamPad, harpsichord]
                    , _instruments=replicate 7 sawOrgan
                    , _enterBeats=16
                    }
  fC <- compileFugue fugue

  guitar <- loop . mul 0.3 <$> loadWav 8 0.93 "samples/MetalRhythm2.wav"
  kcks <- drums "X _ _ _|O _ _ _|O _ _ _|O _ _ _" Tr808.bd2
  snr1 <- drums "_ _ X _|_ _ _ _|_ _ X _|_ _ X _" Hm.sn1
  snr2 <- drums "_ _ _ _|_ _ O _|_ _ _ _|_ _ O _" Hm.sn2
  chhs <- drums "O _ o _|. _ . _|" Tr808.chh
  ohhs <- drums "_ _ _ .|_ _ _ .|" Tr808.ohh
  clps <- drums ". . . .|_ _ _ _|_ _ _ _|_ _ _ _|_ _ _ _|_ _ _ _|_ _ _ _|_ _ _ _|" Hm.clap

  drumEnvs <-
    sequence
    $ getZipList
    $ sqrEnvM
    <$> ZipList [0.0, 0.2, 0.4, 0.5, 0.8, 0.5]
    <*> ZipList [16, 12, 24, 16, 16, 32]


  -- Square envelopes for constantly on before oscillation
  -- constEnvs <- ZipList <$> traverse (\i -> sqrTabEnv [OffFor (i*introN), OnFor introN, OffFor 1024]) [0..20]

  -- Take the max of the two waves to concatenate the square intro.
  let drms = har $ zipEnvsWith (*) drumEnvs [snr1, snr2, ohhs, chhs, clps, kcks]
  return $ har [mul 0.5 <$> drms, fC]

songEnv = SongEnv { _bpm=128
                  , _beatLength=256
                  }

st3' = runSongM songEnv song
st3 = dac =<< st3'
