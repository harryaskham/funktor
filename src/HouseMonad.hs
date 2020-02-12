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

song :: (MonadReader Bpm m, MonadSE m) => m (Seg Sig2)
song = do
  let padNotes = Pch <$> (take 32 . cycle $ minorChord C) ?? 5 ?? 0.3 ?? 8
      leadNotes = take 64 . cycle $ [Pch C 6 0.4 0.5, Silent 0.5]
  kcks <- compileD $ DrumTab "X _ _ _ _ _ . _|o _ _ _ _ _ _ _|o _ _ _ _ _ . _|o _ _ _ _ _ _ _" Tr808.bd
  snrs <- compileD $ DrumTab "_ _ _ _ _ _ _ _|o _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|X _ _ _ _ _ _ _" Tr808.sn
  chhs <- compileD $ DrumTab "_ _ _ _ . _ _ _|_ _ _ _ . _ _ _|_ _ _ _ O _ _ _|_ _ _ _ o _ _ _" Tr808.chh
  ohhs <- compileD $ DrumTab "O _ . _ . _ . _|o _ . _ . _ . _|X _ . _ . _ . _|o _ . _ . _ . _" Tr808.ohh
  pad <- compileI razorPad padNotes
  lead <- compileI polySynth leadNotes
  intro <- cotHar [kcks, snrs, pure lead]
  verse <- cotHar [kcks, snrs, chhs, pure pad]
  chorus <- cotHar [kcks, snrs, ohhs, chhs]
  -- TODO: Something is stopping this from working.
  -- Loop is not doing what I want it to.
  -- STILL STOPS AFTER 32 BEATS WHYYY
  -- Seems to be related to the pad length, which is insane because this isn't supposed to be playing yet.
  -- Something is going wrong somewhere.
  cotraverse mel
    [ forBeats 32 intro
    , forBeats 32 verse
    , forBeats 32 chorus
    , forBeats 32 verse
    ]

hmo = runSeg <$> runReaderT (song :: SongM) 112
