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

-- Global controls + derivations
-- TODO: Might also need to pair this with 'setBpm x >> song'
bpm = 140
bps = bpm / 60
spb = 1 / bps

-- Run the given song respecting the global BPM.
run :: SE Sig2 -> IO()
run song = dac $ setBpm bpm >> song

-- AVAILABLE TR808 DRUMS
-- bd bd2 sn ohh chh htom mtom ltom cym cl rim mar hcon lcon

-- DnB riddim
dnbKicks = DrumTab "O _ o _|_ _ _ _|_ _ O _|_ _ _ _" bd
dnbSnare = DrumTab "_ _ _ _|O _ _ .|_ . _ _|o _ _ ." sn
dnbChats = DrumTab "_ _ _ _|_ _ o _|o _ _ _|_ _ . _" chh
dnbOhats = DrumTab "_ . _ _|_ _ _ _|_ _ _ _|_ _ _ _" ohh

dnbTabs = [dnbKicks, dnbSnare, dnbChats, dnbOhats]
dnbDrums = compileTabs bpm dnbTabs
dnbSong = sum [pure minMel, dnbDrums]

-- sequences = bass >> snares / hat >> full song >> fade out
-- TODO: DNB sequencer that brings in one at a time, then sustains, then drops em out one
--       at a time. Like x bars incoming, many bars sustained, few bars dropoff
-- TODO: Use >>= binding to generate all combinations of drums and play through on loop
-- TODO: Tab generation (all possible tabs) for randomized beats.

-- Minimal song
minKick  = DrumTab "O _ _ _|o _ _ _|o _ _ _|o _ _ _" bd
minSnare = DrumTab "_ _ _ O|_ _ _ _|_ _ _ o|_ _ _ _" sn
minChats = DrumTab "O o o .|" chh

minDrums :: SE Sig2
minDrums = compileTabs bpm [minKick, minSnare, minChats]

minMel :: Sig2
minMel = compileMelodyP vibraphone1 $ str (1/2) combined
  where
    loop1 = loopBy 4 $ toMel ([Pch C, Pch F, Pch Fs, Pch G] <*> pure 8)
    loop2 = loopBy 4 $ toMel ([Pch C, Pch E, Pch G, Pch Bb] <*> pure 8)
    combined = loopBy 32 $ mel [loop1, loop2]

minSong :: SE Sig2
minSong = inOutFilter <$> sum [pure minMel, minDrums]

-- Modifiers (WIP)

-- Phases in and out over two bars
inOutFilter :: SigSpace a => a -> a
inOutFilter = at (mlp (500 + 4500 * uosc (takt 4)) 0.55)

-- Instrument defn
-- TODO: Neat way of using patches
oscInstr :: D -> SE Sig
oscInstr x = return $ mul (linsegr [0, 0.03, 1, 0.2, 0] 0.1 0) $ osc $ sig x

-- Compiles the given track using the given instrument, ensuring BPM matches.
compileTrack :: (D -> SE Sig) -> Track Sig D -> Sig
compileTrack instr = mix . sco instr . fmap cpspch . str spb

-- Compiles the given track using the given patch.
compileMelodyP :: Patch2 -> Track Sig (D, D) -> Sig2
compileMelodyP patch = mix . atSco patch . fmap cpspch2 . str spb

compileMelody :: Track Sig D -> Sig2
compileMelody = fromMono . compileTrack oscInstr
