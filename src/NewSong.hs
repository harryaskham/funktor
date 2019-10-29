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
newTabs = [newBd2, newSn2, newChh]
newDrumsFx = fmap largeHall2
newDrums = newDrumsFx $ compileTabs newBpm newTabs

newPiano = Segment newBpm fmPiano looped
  where
    notes = [Pch C 7, Pch Bb 8, Pch B 6] <*> pure 0.5 <*> pure 0.5
    looped = loopBy 256 . toMel $ notes

newSegments :: [DelayedSegment]
newSegments = [DelayedSegment newPiano 0]

newSong' :: Song
newSong' = Song newSegments newDrums

newSong :: SE Sig2
newSong = compileSong newSong'

runNewSong = runB newBpm newSong
