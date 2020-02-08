{-# LANGUAGE TemplateHaskell #-}

module Saturday where

import Csound.Base hiding (Tab, clp, Duration)
import Csound.Sam
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

data Env = Env { _bpm :: Bpm
               , _patch :: Patch2
               , _beatDuration :: Sig
               }
makeLenses ''Env

compileWith :: Env -> [Pch] -> Sig2
compileWith env notes = compileTrack (env^.bpm) (env^.patch) (toMel . repeatToBeats (env^.beatDuration) $ notes)

arpNotes = sort $ expandScale [6, 7, 8, 9] (minorChord C) ?? 0.3 ?? 0.5
arpEnv = Env bpm epiano2 16
arpEnvelope = fromMono $ sinEnv bpm 0 (beats 1)
arp = (compileWith arpEnv arpNotes) * arpEnvelope

intro = arp

song = return intro

sat = runB bpm song
