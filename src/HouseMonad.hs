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
-- make ti sound actually good - simple house with variety using the new tools
-- turn into literate song file
-- pitched drums, with FX, movd out to own module so that we can use across songs
-- Song env has more than just bpm
-- named patterns and arps that we can concat and introduce expressively

-- TODO: Move inside song monad
numBeats = 128
root = C

song :: SongM
song = do
  -- Drums
  kcks <- drumsDr "X _ _ _ _ _ . _|o _ _ _ _ _ _ _|o _ _ _ _ _ . _|o _ _ _ _ _ _ _" Tr808.bd 0.5
  snrs <- drumsDr "_ _ _ _ _ _ _ _|o _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|X _ _ _ _ _ _ _" Tr808.sn 0.5
  chhs <- drumsDr "_ _ _ _ . _ _ _|_ _ _ _ . _ _ _|_ _ _ _ O _ _ _|_ _ _ _ o _ _ _" Tr808.chh 0.5
  ohhs <- drumsDr "O _ . _ . _ . _|o _ . _ . _ . _|X _ . _ . _ . _|o _ . _ . _ . _" Tr808.ohh 0.5

  -- Instruments
  pad <-
    compileI razorPad
    $ repeatToBeats numBeats
    $ Pch <$> minorChord root ?? 5 ?? 0.3 ?? 8
  lead <-
    compileI razorLead
    $ repeatToBeats numBeats
    $ ((expandScale [6, 7] (minorScale root) ?? 0.4 ?? 0.5) !!) <$> [0, 4, 2, 7, 5, 13, 12, 11]

  -- Sequences
  -- TODO: this only works with the drums removed - some weird interaction with SE
  -- Is non-deterministic
  let intro = har [kcks, snrs]
      verse = har [kcks, snrs, chhs]
      chorus = har [kcks, snrs, ohhs, chhs]

  -- Song structure
  -- TODO: Something is stopping this from working.
  -- Seems to be related to the pad length, which is insane because this isn't supposed to be playing yet.
  cotraverse mel
    [ forBeats 8 intro
    , forBeats 8 verse
    , forBeats 8 chorus
    , forBeats 8 verse
    ]

hmo = runSongM 128 song
hmod = dac =<< hmo
