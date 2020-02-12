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

gBPM = 128
numBeats = 32

compileD = fmap (loop . toSeg) . compileTabs gBPM . pure
kcks = compileD $ DrumTab "X _ _ _ _ _ _ _|O _ _ _ _ _ _ _|O _ _ _ _ _ _ _|O _ _ _ _ _ _ _" Tr808.bd
snrs = compileD $ DrumTab "_ _ _ _ _ _ _ _|O _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|X _ _ _ _ _ _ _" Tr808.sn
ohhs = compileD $ DrumTab ". _ _ _ . _ _ _|. _ _ _ . _ _ _|. _ _ _ . _ _ _|. _ _ _ . _ _ _" Tr808.ohh
chhs = compileD $ DrumTab "X _ o _ o _ o _|O _ o _ o _ o _|X _ o _ o _ o _|X _ o _ o _ o _" Tr808.chh

padNotes = Pch <$> (take 4 . cycle $ minorChord C) ?? 5 ?? 0.3 ?? 8
pad = toSeg $ compileWith
        (Env gBPM razorPad $ fromIntegral numBeats)
        padNotes
lead = toSeg $ compileWith
         (Env gBPM polySynth $ fromIntegral numBeats)
         (take 64 $ cycle [Pch C 6 0.4 0.5, Silent 0.5])

song :: (MonadReader Bpm m, MonadSE m) => m (Seg Sig2)
song = do
  intro <- liftSE (cotraverse har [kcks])
  verse <- liftSE (cotraverse har [kcks, snrs, ohhs])
  chorus <- liftSE (cotraverse har [kcks, snrs, ohhs, chhs])
  cotraverse (loop . mel)
    [ forBeatsM 16 intro
    , forBeatsM 16 verse
    , forBeatsM 16 chorus
    , forBeatsM 16 verse
    ]

hmo = runB gBPM (runSeg <$> runReaderT (song :: SongM (Seg Sig2)) gBPM)
