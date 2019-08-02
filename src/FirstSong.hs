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

bpm :: Bpm
bpm = 174

run :: SE Sig2 -> IO ()
run = runB bpm

-- AVAILABLE TR808 DRUMS
-- bd bd2 sn ohh chh htom mtom ltom cym cl rim mar hcon lcon

-- DnB riddim
dnbKicks = DrumTab "O _ o _|O _ _ _|O _ o _|O _ _ _" Hm.bd2
dnbSnare = DrumTab "_ _ _ _|O _ _ .|_ . _ _|o _ _ ." Hm.sn1
dnbChats = DrumTab ". _ . _|. _ O _|O _ . _|. _ . _" Hm.chh
dnbOhats = DrumTab "_ . _ .|_ . _ .|_ . _ .|_ . _ ." Hm.ohh

dnbTabs = [dnbChats, dnbKicks, dnbSnare, dnbOhats]
dnbDrums = compileTabs bpm dnbTabs
increasingDnbDrums = compileTabSequenceWithLoop bpm 64 $ increasingSequences dnbTabs
dnbSong = sum [pure minBass, pure minMel2, increasingDnbDrums]

-- TODO: Tab generation (all possible tabs) for randomized beats.
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

increasingMinDrums :: SE Sig2
increasingMinDrums = compileTabSequence bpm 64 $ increasingSequences minTabs

minMel :: Sig2
minMel = compileMelody $ Segment bpm guitar combined
  where
    loop1 = toMel (Pch <$> [C, F, Fs, G] <*> [7, 8] <*> [0.5] <*> [1/2, 1/2])
    loop2 = toMel (Pch <$> [C, E, G, Bb] <*> [7, 8] <*> [0.5] <*> [1/4, 1/4, 1/4, 1/4])
    combined = loopBy 32 $ mel [loop1, loop2]

minMel2 :: Sig2
minMel2 = compileMelody $ Segment bpm guitar combined
  where
    loop1 = toMel $ (\o -> Pch <$> [B, Fs, E, Ab, C, Db] <*> [o] <*> [0.5] <*> [1 / 2]) =<< [8, 9, 6, 7]
    combined = loopBy 32 $ mel [loop1]

minBass :: Sig2
minBass = compileMelody $ Segment bpm simpleBass . loopBy 400 . toMel $ Pch <$> [C, E, C, G] <*> [5] <*> [0.8] <*> [1]

minSong :: SE Sig2
minSong = sum [pure minMel2, pure minBass, increasingMinDrums]

-- TODO: finish mega cheesy house experiment
-- Put some effects on the drums
-- Inject bits of silence into the drums
-- Figure out a vocal sample
-- Notion of 'verse' / 'chorus' / 'verse' structure to compose
-- Explore way more instrument types, and experiment with adding effects to get new ones
houseBpm = 128
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

-- Chords that persist in the background.
housePad = Segment houseBpm dreamPad notes
  where
    chord1 = toChord $ Pch <$> [C, Eb, G] <*> [6] <*> [0.5] <*> [8]
    --chord2 = toChord $ Pch <$> [F, Ab, C] <*> [6] <*> [0.5] <*> [8]
    silence = toMel [Silent 24]
    notes = loopBy 128 $ mel [chord1, silence]

-- A less frequent bar of notes to kick in kind of soon.
houseTinkle = Segment houseBpm overtoneLead melody
  where
    notes = Pch <$> [C, Bb, Eb, F, G, Ab, D, C] <*> [8] <*> [0.9] <*> [1]
    melody = loopBy 32 . mel $ [toMel notes, toMel [Silent 32]]

-- An arpeggio that kicks in and persists
houseArp = Segment houseBpm banyan melody
  where
    notes = toMel $ Pch <$> reverse [C, Bb, Eb, F, G, Ab, D, C] <*> [7, 8] <*> [0.7] <*> [1/2]
    silence = toMel [Silent 8]
    melody = loopBy 32 $ mel [notes, notes, silence]

-- TODO: Need a "song interface" where we chop it into bits and can sequence the song together
-- and ideally jump to parts of it easily.
-- Like a Song literal.

houseSong :: SE Sig2
houseSong = head houseDrums + sum (pure . compileMelody <$> melodies)
  where
    melodies = [housePad, withDelay 64 houseTinkle, withDelay 128 houseArp]

-- TODO: Some chords for Tetris to make things sounds a lil nicer.
-- Plus compression effects for an 8-bit sound.

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
tetrisLead = compileMelody $ Segment houseBpm overtoneLead $ loopBy 32 $ toMel tetrisNotes
tetrisHouseSong = sum [head houseDrums, inOutFilter $ pure tetrisLead]
tetrisDnbSong = sum [dnbDrums, pure $ compileMelody $ Segment (bpm/2) razorLead $ loopBy 8 $ toMel tetrisNotes]
