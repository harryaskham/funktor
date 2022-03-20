{-# LANGUAGE FlexibleContexts #-}

module HouseMonad where

import Control.Lens
import Control.Monad.Random
import Control.Monad.Reader
import Csound.Base hiding (Duration, Tab, clp)
import Csound.Catalog.Drum.Tr808 as Tr808
import Csound.Catalog.Effect
import Csound.Patch
import Csound.Sam
import Data.List
import Data.Ord
import Data.Sort
import Melody
import Note
import System.IO.Unsafe
import Tabs
import Tools

-- TODO:
-- fix the issues around segments overlappin
-- add drops back in
-- make ti sound actually good - simple house with variety using the new tools
-- turn into literate song file
-- pitched drums, with FX, movd out to own module so that we can use across songs
-- Song env has more than just bpm
-- named patterns and arps that we can concat and introduce expressively

root = C

song :: SongM
song = do
  numBeats <- asks (view beatLength)

  -- Drums with 20% dropout
  kcks <- drumsDr (concat $ replicate 8 "X _ _ _ _ _ . _|o _ _ _ _ _ _ _|o _ _ _ _ _ . _|o _ _ _ _ _ _ _|") Tr808.bd 0.2
  snrs <- drumsDr (concat $ replicate 8 "_ _ _ _ _ _ _ _|o _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|X _ _ _ _ _ _ _|") Tr808.sn1 0.2
  chhs <- drumsDr (concat $ replicate 8 "_ _ _ _ . _ _ _|_ _ _ _ . _ _ _|_ _ _ _ O _ _ _|_ _ _ _ o _ _ _|") Tr808.chh 0.2
  ohhs <- drumsDr (concat $ replicate 8 "O _ . _ . _ . _|o _ . _ . _ . _|X _ . _ . _ . _|o _ . _ . _ . _|") Tr808.ohh 0.2

  -- Instruments
  pad <-
    compileI razorPad $
      repeatToBeats numBeats $
        Pch <$> minorChord root ?? 5 ?? 0.3 ?? 8
  lead <-
    compileI razorLead $
      repeatToBeats numBeats [Pch root 6 0.4 0.5, Silent 0.5]

  -- Sequences
  let intro = har [kcks, snrs, lead]
      verse = har [kcks, snrs, chhs, pad]
      chorus = har [kcks, snrs, ohhs, chhs, lead, pad]

  -- Song structure
  -- TODO: Something is stopping this from working.
  -- Seems to be related to the pad length, which is insane because this isn't supposed to be playing yet.
  cotraverse
    mel
    [ forBeats 8 intro,
      forBeats 8 verse,
      forBeats 8 chorus,
      forBeats 8 verse
    ]

songEnv =
  SongEnv
    { _bpm = 112,
      _beatLength = 128
    }

hmo = runSongM songEnv song

hmod = dac =<< hmo
