module FirstSong where

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

-- TODO: Once in a state to split up, do so into commented composable utils

bpm :: Bpm
bpm = 180

run :: SE Sig2 -> IO ()
run = runB bpm

-- AVAILABLE TR808 DRUMS
-- bd bd2 sn ohh chh htom mtom ltom cym cl rim mar hcon lcon

-- DnB riddim
dnbKicks = DrumTab "O _ o _|O _ _ _|O _ o _|O _ _ _" Tr808.bd
dnbSnare = DrumTab "_ _ _ _|O _ _ .|_ . _ _|o _ _ ." Tr808.sn
dnbChats = DrumTab ". _ . _|. _ O _|O _ . _|. _ . _" Tr808.chh
dnbOhats = DrumTab "_ . _ _|_ _ _ _|_ _ _ _|_ _ _ _" Tr808.ohh

dnbTabs = [dnbChats, dnbKicks, dnbSnare, dnbOhats]
dnbDrums = compileTabs bpm dnbTabs
increasingDnbDrums = compileTabSequence bpm 64 $ increasingSequences dnbTabs
dnbSong = sum [pure minBass, pure minMel2, increasingDnbDrums]

-- sequences = bass >> snares / hat >> full song >> fade out
-- TODO: DNB sequencer that brings in one at a time, then sustains, then drops em out one
--       at a time. Like x bars incoming, many bars sustained, few bars dropoff
-- TODO: Use >>= binding to generate all combinations of drums and play through on loop
-- TODO: Tab generation (all possible tabs) for randomized beats.
-- TODO: Experiment with pads
-- TODO: Get the intermediary compiled-tab form of Drums and use these with any kind of generation function
-- TODO: "Humanizer" that combines up small variants on velocity in order to naturalize a sound.

-- Minimal song
minKick  = DrumTab "O _ _ _|o _ _ _|o _ _ _|o _ _ _" Tr808.bd
minSnare = DrumTab "_ _ _ O|_ _ _ _|_ _ _ o|_ _ _ _" Tr808.sn
minChats = DrumTab "O o o .|" Tr808.chh
minOhats = DrumTab "_ _ _ _|_ _ O _|" Tr808.ohh
minHtoms = DrumTab "_ _ o _|_ _ _ _|_ . _ _|_ _ _ _" Tr808.htom
minMtoms = DrumTab "_ _ _ o|_ _ _ _|o _ _ _|_ _ _ ." Tr808.mtom
minLtoms = DrumTab "_ _ _ _|_ _ _ o|_ _ _ _|_ _ O _" Tr808.ltom
minCyms  = DrumTab "_ _ _ _|_ _ _ _|_ _ _ _|_ _ _ O" Tr808.cym
minCls   = DrumTab "_ . _ _|_ . _ _|_ o _ _|_ . _ _" Tr808.cl

minTabs :: [DrumTab]
minTabs = [minKick, minSnare, minChats, minOhats, minHtoms, minLtoms, minCyms, minCls]

minDrums :: SE Sig2
minDrums = compileTabs bpm minTabs

-- TODO: Attempt at progressive drums - start just by growing permutations of drums using order

-- The minDrums in increasing order.
increasingMinDrums :: SE Sig2
increasingMinDrums = compileTabSequence bpm 64 $ increasingSequences minTabs

-- NOT DONE YET
-- END DRUMS

minMel :: Sig2
minMel = compileMelody bpm guitar combined
  where
    loop1 = toMel (Pch <$> [C, F, Fs, G] <*> [7, 8] <*> [0.5] <*> [1/2, 1/2])
    loop2 = toMel (Pch <$> [C, E, G, Bb] <*> [7, 8] <*> [0.5] <*> [1/4, 1/4, 1/4, 1/4])
    combined = loopBy 32 $ mel [loop1, loop2]

minMel2 :: Sig2
minMel2 = compileMelody bpm guitar combined
  where
    loop1 = toMel $ (\o -> Pch <$> [B, Fs, E, Ab, C, Db] <*> [o] <*> [0.5] <*> [1 / 2]) =<< [8, 9, 6, 7]
    combined = loopBy 32 $ mel [loop1]

-- TODO
minBass :: Sig2
minBass = compileMelody bpm simpleBass . loopBy 400 . toMel $ Pch <$> [C, E, C, G] <*> [5] <*> [0.8] <*> [1]

minSong :: SE Sig2
minSong = sum [pure minMel2, pure minBass, increasingMinDrums]

-- Modifiers (WIP)

-- TODO: finish mega cheesy house experiment
-- Put some effects on the drums
-- Inject bits of silence into the drums
-- Figure out a vocal sample
-- Notion of 'verse' / 'chorus' / 'verse' structure to compose
houseBpm = 140
houseBd2 = DrumTab "O _ _ _|O _ _ _|O _ _ _|O _ _ _" Hm.bd2
houseSn1 = DrumTab "_ _ _ O|_ _ o _|_ _ _ O|_ _ o _|_ _ _ O|_ _ o _|_ O _ O|_ _ o _" Hm.sn1
houseSn2 = DrumTab "_ _ _ _|_ _ _ _|_ _ _ _|_ _ O O" Hm.sn2
houseChh = DrumTab ". _ . _|. _ . _|. _ . _|. _ . _" Hm.chh
houseOhh = DrumTab "_ o _ o|_ . _ .|_ O _ o|_ . _ ." Hm.ohh
houseClp = DrumTab "_ _ o _|_ _ _ _|_ _ _ _|_ _ _ _|_ _ . _|_ _ _ _|_ _ _ _|_ _ o _" Hm.clap
houseTabs = [houseBd2, houseSn1, houseSn2, houseChh, houseOhh, houseClp]
allHouseDrums = compileTabs houseBpm houseTabs

houseTabSeqs = [
  -- First build up to full set
  increasingSequences houseTabs,
  -- The drop it down again, but not all the way, and not building up all the way
  drop 2 $ increasingSequences houseTabs,
  -- Then don't drop down much at all
  drop 3 $ increasingSequences houseTabs]

houseDrums = compileTabSequenceWithLoop houseBpm 128 <$> houseTabSeqs

-- TODO: Need a newtype for these melodies which we can compile at song creation time
-- Means we can stay in Mel-space for longer and e.g. introduce intro-delays later on.

housePad = compileMelody houseBpm dreamPad notes
  where
    chord1 = toChord $ Pch <$> [C, Eb, G] <*> [6] <*> [1.0] <*> [8]
    chord2 = toChord $ Pch <$> [F, Ab, C] <*> [6] <*> [1.0] <*> [8]
    silence = toMel [Silent 8]
    notes = loopBy 128 $ mel [chord1, silence, chord2, silence]

houseTinkle = compileMelody houseBpm overtoneLead $ mel [introSilence, melody]
  where
    introSilence = toMel [Silent 64]
    notes = Pch <$> [C, Bb, Eb, F, G, Ab, D, C] <*> [8] <*> [1.0] <*> [1]
    melody = loopBy 128 . mel $ [toMel notes, toMel [Silent 32]]

houseArp = compileMelody houseBpm banyan $ mel [introSilence, melody]
  where
    introSilence = toMel [Silent 128]
    notes = Pch <$> reverse [C, Bb, Eb, F, G, Ab, D, C] <*> [7, 8] <*> [0.7] <*> [1/2]
    melody = loopBy 128 $ toMel notes

debugHouseSong = sum [allHouseDrums, pure housePad, pure houseTinkle, pure houseArp]
houseSong = sum [head houseDrums, pure housePad, pure houseTinkle, pure houseArp]

--

tetrisNotes1 :: [Pch]
tetrisNotes1 =
  getZipList $
  Pch <$>
  ZipList [E, B, C, D, C, B, A, C, E, D, C, B, B, C, D, E, C, A, A] <*>
  ZipList [8, 7, 8, 8, 8, 7, 7, 8, 8, 8, 8, 7, 7, 8, 8, 8, 8, 7, 7] <*>
  ZipList (repeat 1.0) <*>
  ZipList ([2, 1, 1, 2, 1, 1, 3, 1, 2, 1, 1, 2, 1, 1, 2, 2, 2, 2, 5] <**> pure (*0.25))

tetrisNotes2 :: [Pch]
tetrisNotes2 =
  getZipList $
  Pch <$>
  ZipList [D, F, A, G, F, E, C, E, D, C, B, B, C, D, E, C, A, A]  <*>
  ZipList [8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 7, 7, 8, 8, 8, 8, 7, 7] <*>
  ZipList (repeat 1.0) <*>
  ZipList ([2, 1, 2, 1, 1, 3, 1, 2, 1, 1, 2, 1, 1, 2, 2, 2, 2, 4] <**> pure (*0.25))

tetrisNotes3 :: [Pch]
tetrisNotes3 =
  getZipList $
  Pch <$>
  ZipList [E, C, D, B, C, A, Ab, E, C, D, B, C, E, A, A, Ab]  <*>
  ZipList [8, 8, 8, 7, 8, 7, 7, 8, 8, 8, 7, 8, 8, 8, 8, 8] <*>
  ZipList (repeat 1.0) <*>
  ZipList ([4, 4, 4, 4, 4, 4, 8, 4, 4, 4, 4, 2, 2, 2, 2, 8] <**> pure (*0.25))

tetrisNotes = concat [tetrisNotes1, tetrisNotes2, tetrisNotes1, tetrisNotes2, tetrisNotes3]
tetrisLead = compileMelody houseBpm overtoneLead $ loopBy 32 $ toMel tetrisNotes
tetrisHouseSong = sum [head houseDrums, inOutFilter $ pure tetrisLead]
