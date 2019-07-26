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

-- TODO: Once in a state to split up, do so into commented composable utils

bpm = 180

-- AVAILABLE TR808 DRUMS
-- bd bd2 sn ohh chh htom mtom ltom cym cl rim mar hcon lcon

-- DnB riddim
dnbKicks = DrumTab "O _ o _|_ _ _ _|_ _ O _|_ _ _ _" bd
dnbSnare = DrumTab "_ _ _ _|O _ _ .|_ . _ _|o _ _ ." sn
dnbChats = DrumTab "_ _ _ _|_ _ o _|o _ _ _|_ _ . _" chh
dnbOhats = DrumTab "_ . _ _|_ _ _ _|_ _ _ _|_ _ _ _" ohh

dnbTabs = [dnbKicks, dnbSnare, dnbChats, dnbOhats]
dnbDrums = compileTabs bpm dnbTabs
dnbSong = sum [pure minBass, dnbDrums]

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
increasingMinDrums :: [[DrumTab]]
increasingMinDrums = increasingSequences minTabs

compileIncreasing :: [[DrumTab]] -> SE [Sig2]
compileIncreasing = traverse (compileTabs bpm)

-- TODO: This won't play, why
--debugDrums :: SE Sig2
--debugDrums = sequenced
--  where
--    compiled = compileIncreasing increasingMinDrums
--    limited = (fmap . fmap) (limSig 10) compiled
--    sequenced = flow $ (fmap . fmap) toSeg limited

-- NOT DONE YET
-- END DRUMS

-- Allows us to loop a signal, not just a segment
loopSig :: Sig2 -> Sig2
loopSig = runSeg . loop . toSeg

limSig :: Sig -> Sig2 -> Sig2
limSig bars = runSeg . constLim (takt bars) . toSeg

minMel :: Sig2
minMel = compileMelody bpm razorLead combined
  where
    loop1 = toMel (Pch <$> [C, F, Fs, G] <*> [7, 8] <*> [0.5] <*> [1/2, 1/2])
    loop2 = toMel (Pch <$> [C, E, G, Bb] <*> [7, 8] <*> [0.5] <*> [1/4, 1/4, 1/4, 1/4])
    combined = loopBy 32 $ mel [loop1, loop2]

-- TODO
minBass :: Sig2
minBass = compileMelody bpm simpleBass . loopBy 4 . toMel $ Pch <$> [C, E, C, G] <*> [5] <*> [0.8] <*> [1]

minSong :: SE Sig2
minSong = sum [pure minMel, pure minBass, minDrums]

-- Modifiers (WIP)

-- Phases in and out over two bars
inOutFilter :: SigSpace a => a -> a
inOutFilter = at (mlp (500 + 4500 * uosc (takt 4)) 0.55)

-- Compiles the given track using the given patch.
compileMelody :: BPM -> Patch2 -> Track Sig (D, D) -> Sig2
compileMelody bpm patch = mix . atSco patch . fmap cpspch2 . str (spb bpm)
