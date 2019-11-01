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
newSn2 = DrumTab "_ _ o _|_ _ _ _|" Hm.sn2
newChh = DrumTab "o . . .|" Hm.chh
newTabSeqs = [ [newBd2]
             , [newBd2, newSn2]
             , [newBd2, newSn2, newChh] ]
newDrumsFx = fmap smallHall2
newDrums = newDrumsFx $ compileTabSequenceWithLoop newBpm 32 newTabSeqs

newPiano = Segment newBpm razorLead looped
  where
    notes1 = Pch <$> [C, Eb, G, Eb] <*> pure 7 <*> pure 0.5 <*> pure 0.25
    notes2 = Pch <$> [F, F, G, Bb] <*> pure 7 <*> pure 0.5 <*> pure 0.25
    looped = loopBy 128 . toMel $ notes1 ++ [Silent 3] ++ notes2 ++ [Silent 3]

newSong' :: Song
newSong' = Song [ DelayedDrums newDrums 16
                , DelayedSegment newPiano 0 ]

newSong :: SE Sig2
newSong = compileSong newSong'

runNewSong = runB newBpm newSong
