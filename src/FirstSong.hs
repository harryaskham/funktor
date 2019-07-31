module FirstSong where

import Csound.Base hiding (Tab)
import Csound.Catalog.Drum.Tr808
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
dnbKicks = DrumTab "O _ o _|O _ _ _|O _ o _|O _ _ _" bd
dnbSnare = DrumTab "_ _ _ _|O _ _ .|_ . _ _|o _ _ ." sn
dnbChats = DrumTab ". _ . _|. _ O _|O _ . _|. _ . _" chh
dnbOhats = DrumTab "_ . _ _|_ _ _ _|_ _ _ _|_ _ _ _" ohh

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
minKick  = DrumTab "O _ _ _|o _ _ _|o _ _ _|o _ _ _" bd
minSnare = DrumTab "_ _ _ O|_ _ _ _|_ _ _ o|_ _ _ _" sn
minChats = DrumTab "O o o .|" chh
minOhats = DrumTab "_ _ _ _|_ _ O _|" ohh
minHtoms = DrumTab "_ _ o _|_ _ _ _|_ . _ _|_ _ _ _" htom
minMtoms = DrumTab "_ _ _ o|_ _ _ _|o _ _ _|_ _ _ ." mtom
minLtoms = DrumTab "_ _ _ _|_ _ _ o|_ _ _ _|_ _ O _" ltom
minCyms  = DrumTab "_ _ _ _|_ _ _ _|_ _ _ _|_ _ _ O" cym
minCls   = DrumTab "_ . _ _|_ . _ _|_ o _ _|_ . _ _" cl

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

-- Phases in and out over two bars
inOutFilter :: SigSpace a => a -> a
inOutFilter = at (mlp (500 + 4500 * uosc (takt 4)) 0.55)

-- TODO: Mega cheesy house.
-- Starts with simple 4-4 bass.
-- Then kicks.
-- Then apocalyptic BWAOOOOO
-- Then kicks.
-- Then BWAOOOOO
-- etc
-- then drum build up
-- bass build up
-- lyrical sample
-- deep deep drop of some sort with a solid bottom
-- repeat
-- TRACK
