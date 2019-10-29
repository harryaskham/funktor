module NewSong where

import Csound.Base hiding (Tab)
import qualified Csound.Catalog.Drum.Tr808 as Tr808
import qualified Csound.Catalog.Drum.Hm as Hm
import Csound.Patch
import Csound.Sam
import Csound.Sam.Core
import Data.List.Split
import Tabs
import Tools
import Note
import Melody

newBpm = 110
newBd2 = DrumTab "O _ _ _|o _ _ _|o _ _ _|o _ _ _" Hm.bd2
newSn2 = DrumTab "O _ _ _|_ _ _ _|" Hm.sn2
newChh = DrumTab "o . . .|" Hm.chh
newTabs = [newBd2, newSn2]
newDrums = compileTabs newBpm newTabs

-- Chords that persist in the background.
newPad = Segment newBpm dreamPad notes
  where
    chord1 = toChord $ Pch <$> [C, Eb, G] <*> [6] <*> [0.5] <*> [8]
    --chord2 = toChord $ Pch <$> [F, Ab, C] <*> [6] <*> [0.5] <*> [8]
    silence = toMel [Silent 24]
    notes = loopBy 128 $ mel [chord1, silence]

newSegments :: [DelayedSegment]
newSegments = []

newSong' :: Song
newSong' = Song newSegments newDrums

newSong :: SE Sig2
newSong = compileSong newSong'

runNewSong = runB newBpm newSong
