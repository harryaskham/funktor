module FirstSong where

import Csound.Base hiding (Tab)
import Csound.Catalog.Drum.Tr808
import Csound.Patch
import Csound.Sam
import Csound.Sam.Core
import Data.List.Split
import Melodies
import Tabs

-- TODO: Once in a state to split up, do so into commented composable utils

-- Global controls + derivations
-- TODO: Might also need to pair this with 'setBpm x >> song'
bpm = 140
bps = bpm / 60
spb = 1 / bps

-- AVAILABLE TR808 DRUMS
-- bd bd2 sn ohh chh htom mtom ltom cym cl rim mar hcon lcon

-- Takes a list of drum tracks and compiles to a signal.
compileSample :: Sample Sig2 -> SE Sig2
compileSample = runSam (bpm * 4)

-- Takes those tabs and turns em into musak
compileTabs :: [DrumTab] -> SE Sig2
compileTabs tabs = compileSample . sum $ compileTab <$> tabs

-- Basic first pass
basicKicks = DrumTab "O _ _ _|. _ _ _|o _ _ _|. _ _ _" bd
basicHatss = DrumTab "_ _ _ o|O _ _ _" chh
basicSnare = DrumTab "O _ _ _|. _ _ _" sn
basicDrums = compileSample . sum $ compileTab <$> [basicKicks, basicHatss, basicSnare]

-- DnB riddim
dnbKicks = DrumTab "O _ o _|_ _ _ _|_ _ O _|_ _ _ _" bd
dnbSnare = DrumTab "_ _ _ _|O _ _ .|_ . _ _|o _ _ ." sn
dnbChats = DrumTab "_ _ _ _|_ _ o _|o _ _ _|_ _ . _" chh
dnbOhats = DrumTab "_ . _ _|_ _ _ _|_ _ _ _|_ _ _ _" ohh

dnbTabs = [dnbKicks, dnbSnare, dnbChats, dnbOhats]
dnbDrums = compileSample . sum $ compileTab <$> dnbTabs

-- sequences = bass >> snares / hat >> full song >> fade out
-- TODO: DNB sequencer that brings in one at a time, then sustains, then drops em out one at a time. Like x bars incoming, many bars sustained, few bars dropoff

-- Minimal drums
minKick = DrumTab "O____|o___|o___|o___"


-- Instrument defn
oscInstr :: D -> SE Sig
oscInstr x = return $ mul (linsegr [0, 0.03, 1, 0.2, 0] 0.1 0) $ osc $ sig x

-- Compiles the given track using the given instrument, ensuring BPM matches.
-- TODO: Extend to non-mono, non-simple instruments
compileTrack :: (D -> SE Sig) -> Track Sig D -> Sig
compileTrack instr = mix . sco instr . fmap cpspch . str spb

melody = fromMono . compileTrack oscInstr $ twinkle

song = sum [pure melody, dnbDrums]
