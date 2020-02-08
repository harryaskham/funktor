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
-- Could also do this within a Song monad that contains info about bpm

bpm = 140

arp = (compileTrack bpm epiano1 $ (toMel . repeatToBeats 16) (sort $ expandScale [6, 7, 8, 9] (minorChord C) ?? 0.3 ?? 0.5)) * (fromMono $ sinEnv bpm 0 (beats 1))

intro = arp

song = return intro

sat = runB bpm song
