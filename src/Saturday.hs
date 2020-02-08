module Saturday where

import Csound.Base hiding (Tab, clp, Duration)
import Csound.Patch
import Tabs
import Tools
import Note
import Melody
import Data.Sort
import Data.Ord

-- LITERATE MUSIC
-- Simple intro, pad, and then some dark house
-- Simplify language where possible, but not DSL

bpm = 140

arp = compileTrack bpm epiano1 $ (toMel . repeatToBeats 16) (sort $ expandScale [6, 7, 8, 9] (minorChord C) ?? 0.3 ?? 0.5)

intro = arp

song = return intro

sat = runB bpm song
