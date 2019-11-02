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

-- TODO: segment should have a duration in beats

bd2 = DrumTab "O _ _ _|o _ _ _|o _ _ _|o _ _ _" Hm.bd2
sn2 = DrumTab "_ _ _ o|_ . _ _|" Hm.sn2
chh = DrumTab ". . _ _|" Hm.chh
ohh = DrumTab "_ _ O _|" Hm.ohh
tabs = [ [bd2]
       , [chh]
       , [ohh]
       , [sn2] ]
drumsFx = fmap smallRoom2
compiledDrums = ZipList $ drumsFx . compileTabs bpm <$> tabs
drumDelays = ZipList [bars 4, bars 8, bars 12, bars 16]
drums = getZipList $ DelayedDrums <$> compiledDrums <*> drumDelays

chords = Segment bpm overtoneLead looped
  where
    chord1 = toChord $ Pch <$> [C, Eb, G] <*> pure 8 <*> pure 1.0 <*> pure 4
    chord2 = toChord $ Pch <$> [F, Ab, B] <*> pure 8 <*> pure 1.0 <*> pure 4
    gap = toMel [ Silent 4 ]
    looped = loopBy 32 . mel $ [chord1, gap, chord2, gap]

piano = Segment bpm razorLead looped
  where
    notes' = ZipList $ Pch <$> [G, Bb, Ab, G, C, Eb, C] <*> pure 7 <*> pure 0.8
    notes = getZipList $ notes' <*> ZipList [1/4, 1/4, 1/2, 1/3, 1/3, 1/3]
    looped = loopBy 32 . toMel $ notes ++ [Silent 6]

song' :: Song
song' = Song bpm $ drums ++ [ DelayedSegment chords 0
                            , DelayedSegment piano $ bars 16 ]

song :: SE Sig2
song = compileSong song'

rs = runSong song'
