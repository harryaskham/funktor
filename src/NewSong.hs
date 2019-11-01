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
newChh = DrumTab ". . _ _|" Hm.chh
newOhh = DrumTab "_ _ O _|" Hm.ohh
newTabs = [ [newChh, newOhh]
          , [newBd2]
          , [newSn2] ]
newDrumsFx = fmap largeHall2
newCompiledDrums = ZipList $ newDrumsFx . compileTabs newBpm <$> newTabs
newDrumDelays = ZipList [0, 4, 8]
newDrums = getZipList $ DelayedDrums <$> newCompiledDrums <*> newDrumDelays

newLead = Segment newBpm 

newPiano = Segment newBpm razorLead looped
  where
    notes1 = Pch <$> [C, Eb, G, Eb] <*> pure 7 <*> pure 0.5 <*> pure 0.25
    notes2 = Pch <$> [F, F, G, Bb] <*> pure 7 <*> pure 0.5 <*> pure 0.25
    looped = loopBy 128 . toMel $ notes1 ++ [Silent 3] ++ notes2 ++ [Silent 3]

newSong' :: Song
newSong' = Song $ newDrums ++ [ DelayedSegment newPiano 8 ]

newSong :: SE Sig2
newSong = compileSong newSong'

runNewSong = runB newBpm newSong
