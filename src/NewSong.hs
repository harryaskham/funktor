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

bpm = 120

bd2 = DrumTab "O _ _ _|o _ _ _|o _ _ _|o _ _ _" Hm.bd2
sn2 = DrumTab "_ _ _ o|_ _ o _|" Hm.sn2
chh = DrumTab ". . _ _|" Hm.chh
ohh = DrumTab "_ _ O _|" Hm.ohh
tabs = [ [bd2]
       , [chh]
       , [ohh]
       , [sn2] ]
drumsFx = fmap smallRoom2
compiledDrums = ZipList $ drumsFx . compileTabs bpm <$> tabs
drumDelays = ZipList $ SegDelay <$> [bars 4, bars 8, bars 12, bars 16]
drumDurations = ZipList $ SegDuration <$> [bars 4, bars 4, bars 4, bars 4]
drums = getZipList $ DelayedDrums <$> compiledDrums <*> drumDelays <*> drumDurations

chords = Segment bpm nightPad looped
  where
    chord1 = toChord $ Pch <$> [C, Eb, G] <*> pure 6 <*> pure 1.0 <*> pure 12
    chord2 = toChord $ Pch <$> [F, Ab, B] <*> pure 6 <*> pure 1.0 <*> pure 12
    gap = toMel [ Silent 4 ]
    looped = loopBy 32 . mel $ [chord1, gap, chord2, gap]

song' :: Song
song' = Song bpm $ drums ++ [ DelayedSegment chords (SegDelay 0) (SegDuration 64) ]

song :: SE Sig2
song = compileSong song'

rs = runSong song'
