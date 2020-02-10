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
import Control.Lens
import qualified Csound.Catalog.Drum as Drum
import Csound.Catalog.Drum.MiniPops as Mp
import Csound.Catalog.Effect

-- LITERATE MUSIC
-- Simple intro, pad, and then some dark house
-- Simplify language where possible, but not DSL
-- Could also do this within a Song monad that contains info about bpm

data Env = Env { _bpm :: Bpm
               , _patch :: Patch2
               , _beatDuration :: Duration
               }
makeLenses ''Env

compileWith :: Env -> [Pch] -> Sig2
compileWith env notes = compileTrack (env^.bpm) (env^.patch) (toMel . repeatToBeats (env^.beatDuration) $ notes)

compileD = compileTabs gBPM . pure

forBeats :: Sig -> SE Sig2 -> SE Sig2
forBeats n = fmap (limSig (Beats gBPM n))

gBPM = 128
numBeats = 32

kick = compileD $ DrumTab "X _ _ _ _ _ _ _|O _ _ _ _ _ _ _|O _ _ _ _ _ _ _|O _ _ _ _ _ _ _" Mp.bd numBeats
cows = compileD $ DrumTab "O _ . _ . _ . _|_ _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|_ _ _ _ _ _ _ _" Mp.cow numBeats
snar = compileD $ DrumTab "_ _ _ _ _ _ o _|_ _ _ _ o _ _ _|_ _ o _ _ _ o _|_ _ _ _ O _ _ _" Mp.sn1 numBeats
sna2 = compileD $ DrumTab "_ _ o _ o _ _ _|_ _ O _ _ _ _ _|_ _ _ _ X _ o _|_ _ O _ _ _ X _" Mp.sn2 numBeats
cyms = compileD $ DrumTab ". _ . _ . _ . _|. . . . . . . .|. . . . . . . .|. . . . . . . ." Mp.cym1 numBeats
cymt = compileD $ DrumTab "X _ o _ o _ o _|O _ o _ o _ o _|X _ o _ o _ o _|X _ o _ o _ o _" Mp.cym2 numBeats
tams = compileD $ DrumTab "_ o _ X _ o _ X|_ _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|_ _ _ _ _ _ _ _" Mp.tam numBeats
gros = compileD $ DrumTab "_ _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|X _ _ _ _ _ _ _|X _ _ _ _ _ _ _" Mp.gro numBeats
mars = compileD $ DrumTab "_ _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|X _ _ _ X _ _ _|X _ _ _ X _ _ _" Mp.mar numBeats
qujs = compileD $ DrumTab "_ _ o _ _ _ _ _|_ _ o _ _ _ _ _|_ _ o _ _ _ _ _|_ _ o _ _ _ _ _" Mp.qj numBeats

padNotes = Pch <$> (take 4 . cycle $ minorChord D) ?? 5 ?? 0.3 ?? 8
pad = compileWith (Env gBPM razorPad $ fromIntegral numBeats) padNotes

intro1 = sum [kick, cows]
intro2 = sum [intro1, snar, qujs]
--intro = sum [kick, cows, snar, sna2, cyms, cymt, tams, gros, mars, qujs, pure pad]

song = sequence [ forBeats 4 intro1
                , forBeats 4 intro2
                ]

sat = runB gBPM (foldl1 concatSig <$> song)
