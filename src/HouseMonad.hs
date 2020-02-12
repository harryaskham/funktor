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

gBPM = 112

compileD = fmap toSeg . compileTabs gBPM . pure
kcks = compileD $ DrumTab "X _ _ _ _ _ . _|o _ _ _ _ _ _ _|o _ _ _ _ _ . _|o _ _ _ _ _ _ _" Tr808.bd
snrs = compileD $ DrumTab "_ _ _ _ _ _ _ _|o _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|X _ _ _ _ _ _ _" Tr808.sn
chhs = compileD $ DrumTab "_ _ _ _ . _ _ _|_ _ _ _ . _ _ _|_ _ _ _ O _ _ _|_ _ _ _ o _ _ _" Tr808.chh
ohhs = compileD $ DrumTab "O _ . _ . _ . _|o _ . _ . _ . _|X _ . _ . _ . _|o _ . _ . _ . _" Tr808.ohh

padNotes = Pch <$> (take 4 . cycle $ minorChord C) ?? 5 ?? 0.3 ?? 8
leadNotes = take 32 . cycle $ [Pch C 6 0.4 0.5, Silent 0.5]
pad = toSeg $ compileWith (Env gBPM razorPad) padNotes
lead = toSeg $ compileWith (Env gBPM polySynth) leadNotes

song :: (MonadReader Bpm m, MonadSE m) => m (Seg Sig2)
song = do
  intro <- liftSE (cotraverse har [kcks, snrs, pure lead])
  verse <- liftSE (cotraverse har [kcks, snrs, chhs, pure pad])
  chorus <- liftSE (cotraverse har [kcks, snrs, ohhs, chhs])
  -- TODO: Something is stopping this from working.
  -- Loop is not doing what I want it to.
  -- STILL STOPS AFTER 32 BEATS WHYYY
  cotraverse (loop . mel)
    [ forBeatsM 4 intro
    , forBeatsM 4 verse
    , forBeatsM 4 chorus
    , forBeatsM 4 verse
    ]

hmo = runB gBPM (runSeg <$> runReaderT (song :: SongM) gBPM)
