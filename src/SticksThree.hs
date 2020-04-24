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

song :: SongM
song = do
  --let theme o v d = Pch <$> [B', A, C, H] ?? o ?? v ?? d
  let theme o v d = Pch <$> [D, F, Bb, A, D, C] ?? o ?? v ?? d
      introN = 16

  voice1 <- compileI hammondOrgan (repeatToBeats 64 $ theme 7 0.3 4)
  voice2 <- compileI hammondOrgan (repeatToBeats 48 $ theme 8 0.3 8)
  voice3 <- compileI hammondOrgan (repeatToBeats 32 $ reverse (theme 7 0.3 3))
  voice4 <- compileI hammondOrgan (repeatToBeats 16 $ theme 9 0.3 16)
  voice5 <- compileI hammondOrgan (repeatToBeats 16 $ reverse (theme 8 0.3 (3/2)))

  kcks <- drums "X _ _ _|O _ _ _|O _ _ _|O _ _ _" Tr808.bd2
  snr1 <- drums "_ _ X _|_ _ _ _|_ _ X _|_ _ X _" Hm.sn1
  snr2 <- drums "_ _ _ _|_ _ O _|_ _ _ _|_ _ O _" Hm.sn2
  chhs <- drums "O _ o _|. _ . _|" Tr808.chh
  ohhs <- drums "_ . _ .|_ . _ .|" Tr808.ohh
  clps <- drums "O O O O|_ _ _ _|_ _ _ _|_ _ _ _" Hm.clap
  guitar <- loop . mul 0.8 <$> loadWav 16 1.02 "samples/doomy.wav"

  -- Sinusoidal envelopes with increasing prime wavelengths
  --primeEnvs <- ZipList . fmap modEnv <$> traverse (sinEnvM 0) (sig . int . fromIntegral <$> take 20 primes)
  primeEnvs <- ZipList . fmap modEnv <$> traverse (sinEnvM 0) [4,8,12,16,24,32]

  --drumEnvs <- traverse (sqrEnvM 0.5) (sig . int . fromIntegral <$> drop 3 (take 20 primes))
  drumEnvs <- traverse (sqrEnvM 0.5) [4,8,12,16,24,32]

  -- Square envelopes for entering voices.
  introEnvs <- ZipList <$> traverse (\i -> sqrTabEnv [OffFor (i*introN), OnFor 1024]) [0..20]

  -- Square envelopes for constantly on before oscillation
  constEnvs <- ZipList <$> traverse (\i -> sqrTabEnv [OffFor (i*introN), OnFor introN, OffFor 1024]) [0..20]

  -- Take the max of the two waves to concatenate the square intro.
  let withIntros = (\e1 e2 -> minabs [e1, e2]) <$> primeEnvs <*> introEnvs
      voiceEnvs = getZipList $ (\e1 e2 -> maxabs [e1, e2]) <$> constEnvs <*> withIntros

  let zipEnvs es ss = getZipList $ (fmap <$> (stereoMap <$> ((*) <$> ZipList es))) <*> ZipList ss
      drms = har $ zipEnvs drumEnvs [chhs, snr2, ohhs, snr1, clps, kcks]
      voices = har $ zipEnvs voiceEnvs [voice1, voice2, voice3, voice4, voice5]

  -- return $ har [drms, voices, guitar]
  return $ har [drms, voices]

songEnv = SongEnv { _bpm=128
                  , _beatLength=1024
                  }

st3' = runSongM songEnv song
st3 = dac =<< st3'
