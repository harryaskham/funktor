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

-- TODO:
-- fix the issues around segments overlappin
-- add drops back in
-- turn into literate song file

song :: SongM
song = do
  -- Drums
  kcks <- drums "X _ _ _ _ _ . _|o _ _ _ _ _ _ _|o _ _ _ _ _ . _|o _ _ _ _ _ _ _" Tr808.bd
  snrs <- drums "_ _ _ _ _ _ _ _|o _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|X _ _ _ _ _ _ _" Tr808.sn
  chhs <- drums "_ _ _ _ . _ _ _|_ _ _ _ . _ _ _|_ _ _ _ O _ _ _|_ _ _ _ o _ _ _" Tr808.chh
  ohhs <- drums "O _ . _ . _ . _|o _ . _ . _ . _|X _ . _ . _ . _|o _ . _ . _ . _" Tr808.ohh

  -- Instruments
  pad <-
    compileI razorPad
    $ Pch <$> (take 32 . cycle $ minorChord C) ?? 5 ?? 0.3 ?? 8
  lead <-
    compileI polySynth
    $ take 64 . cycle
    $ intersperse (Silent 0.5) (Pch <$> minorScale C ?? 6 ?? 0.4 ?? 0.5) ++ [Silent 0.5]

  -- Sequences
  let intro = har [kcks, snrs, lead]
      verse = har [kcks, snrs, chhs, pad]
      chorus = har [kcks, snrs, ohhs, chhs, lead, pad]

  -- Song structure
  -- TODO: Something is stopping this from working.
  -- Seems to be related to the pad length, which is insane because this isn't supposed to be playing yet.
  cotraverse mel
    [ forBeats 8 intro
    , forBeats 8 verse
    , forBeats 8 chorus
    , forBeats 8 verse
    ]

hmo = runSongM 112 song
