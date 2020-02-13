{-# LANGUAGE FlexibleContexts #-}

module HouseMonad where

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
import Csound.Catalog.Effect
import System.IO.Unsafe
import Control.Monad.Reader
import Control.Monad.Random
import Data.List

song :: (MonadReader Bpm m, MonadSE m) => m (Seg Sig2)
song = do
  -- Drums
  kcks <- compileD $ DrumTab "X _ _ _ _ _ . _|o _ _ _ _ _ _ _|o _ _ _ _ _ . _|o _ _ _ _ _ _ _" Tr808.bd
  snrs <- compileD $ DrumTab "_ _ _ _ _ _ _ _|o _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|X _ _ _ _ _ _ _" Tr808.sn
  chhs <- compileD $ DrumTab "_ _ _ _ . _ _ _|_ _ _ _ . _ _ _|_ _ _ _ O _ _ _|_ _ _ _ o _ _ _" Tr808.chh
  ohhs <- compileD $ DrumTab "O _ . _ . _ . _|o _ . _ . _ . _|X _ . _ . _ . _|o _ . _ . _ . _" Tr808.ohh

  -- Instruments
  pad <-
    compileI razorPad
    $ Pch <$> (take 32 . cycle $ minorChord C) ?? 5 ?? 0.3 ?? 8
  lead <-
    compileI polySynth
    $ take 64 . cycle $ intersperse (Silent 0.5) (Pch <$> minorScale C ?? 6 ?? 0.4 ?? 0.5) ++ [Silent 0.5]

  -- Sequences
  intro <- cotHar [kcks, snrs, pure lead]
  verse <- cotHar [kcks, snrs, chhs, pure pad]
  chorus <- cotHar [kcks, snrs, ohhs, chhs, pure lead, pure pad]

  -- Song structure
  -- TODO: Something is stopping this from working.
  -- Seems to be related to the pad length, which is insane because this isn't supposed to be playing yet.
  cotraverse mel
    [ forBeats 8 intro
    , forBeats 8 verse
    , forBeats 8 chorus
    , forBeats 8 verse
    ]

hmo = runSeg . loopBy 2 <$> runReaderT (song :: SongM) 112
