module HouseSong where

import Csound.Base hiding (Tab, clp, segments)
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

-- AVAILABLE TR808 DRUMS
-- bd bd2 sn ohh chh htom mtom ltom cym cl rim mar hcon lcon

-- TODO: Tab generation (all possible tabs) for randomized beats.
-- TODO: "Humanizer" that combines up small variants on velocity in order to naturalize a sound.
-- Put some effects on the drums
-- Inject bits of silence into the drums
-- Figure out a vocal sample
-- Notion of 'verse' / 'chorus' / 'verse' structure to compose
-- Explore way more instrument types, and experiment with adding effects to get new ones
-- A choppy kind of "on-off" filter like end of mt st michael
bpm = 128
bd2 = DrumTab "O _ _ _|O _ _ _|O _ _ _|O _ _ _" Hm.bd2
sn1 = DrumTab "_ _ _ O|_ _ o _|_ _ _ O|_ _ o _|_ _ _ O|_ _ o _|_ O _ O|_ _ o _" Hm.sn1
sn2 = DrumTab "_ _ _ _|_ _ _ _|_ _ _ _|_ _ O O" Hm.sn2
chh = DrumTab ". _ . _|. _ . _|. _ . _|. _ . _" Hm.chh
ohh = DrumTab "_ o _ o|_ . _ .|_ O _ o|_ . _ ." Hm.ohh
clp = DrumTab "_ _ o _|_ _ _ _|_ _ _ _|_ _ _ _|_ _ . _|_ _ _ _|_ _ _ _|_ _ o _" Hm.clap
tabs = [bd2, sn1, sn2, chh, ohh, clp]
alldrums = compileTabs bpm tabs

tabSeqs = [
  -- First build up to full set
  increasingSequences tabs,
  -- The drop it down again, but not all the way, and not building up all the way
  drop 2 $ increasingSequences tabs,
  -- Then don't drop down much at all
  drop 3 $ increasingSequences tabs]

drums = compileTabSequenceWithLoop bpm 128 <$> tabSeqs

-- Chords that persist in the background.
pad = Segment bpm dreamPad notes
  where
    chord1 = toChord $ Pch <$> [C, Eb, G] <*> [6] <*> [0.5] <*> [8]
    --chord2 = toChord $ Pch <$> [F, Ab, C] <*> [6] <*> [0.5] <*> [8]
    silence = toMel [Silent 24]
    notes = loopBy 128 $ mel [chord1, silence]

-- A less frequent bar of notes to kick in kind of soon.
tinkle = Segment bpm overtoneLead melody
  where
    notes = Pch <$> [C, Bb, Eb, F, G, Ab, D, C] <*> [8] <*> [0.9] <*> [1]
    melody = loopBy 32 . mel $ [toMel notes, toMel [Silent 32]]

-- An arpeggio that kicks in and persists
arp = Segment bpm banyan melody
  where
    notes = toMel $ Pch <$> reverse [C, Bb, Eb, F, G, Ab, D, C] <*> [7, 8] <*> [0.7] <*> [1/2]
    silence = toMel [Silent 8]
    melody = loopBy 32 $ mel [notes, notes, silence]

segments :: [DelayedSegment]
segments = [ DelayedDrums (head drums) (SegDelay 0) (SegDuration 256)
           , DelayedSegment pad (SegDelay 0) (SegDuration 256)
           , DelayedSegment tinkle (SegDelay 64) (SegDuration 256)
           , DelayedSegment arp (SegDelay 128) (SegDuration 128) ]

song' :: Song
song' = Song bpm segments

song :: SE Sig2
song = compileSong song'
