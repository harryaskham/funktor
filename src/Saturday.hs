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
import System.IO.Unsafe

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

--compileD = unsafePerformIO . compileWithDropOut 0.1 gBPM
compileD = compileTabs gBPM . pure

forBeats :: (SigSpace a, Sigs a) => Sig -> Seg a -> Seg a
forBeats n = limSig (Beats gBPM n)

catSegs :: [SE (Seg Sig2)] -> SE Sig2
catSegs segs = runSeg . mel <$> sequence segs

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

padNotes = Pch <$> (take 4 . cycle $ minorChord C) ?? 5 ?? 0.3 ?? 8
pad = compileWith (Env gBPM razorPad $ fromIntegral numBeats) padNotes

lead = compileWith (Env gBPM polySynth $ fromIntegral numBeats) (take 64 $ cycle [Pch C 6 0.4 0.5, Silent 0.5])

-- drums = sum [kick, cows, snar, sna2, cyms, cymt, tams, gros, mars, qujs, pure pad]

song = do
  drop <- toSeg <$> qujs
  intro1 <- toSeg <$> sum [kick, cows]
  intro2 <- toSeg <$> sum [kick, cows, snar]
  maindrum <- toSeg <$> sum [kick, cows, snar, mars, tams, sna2]
  let mainlead = toSeg lead
      mainpad = toSeg pad
  return
    $ runSeg . loop . mel
    $ [ (intro1 =:= mainpad) & forBeats 16 & withDrop 4 12 drop
      , intro2 & forBeats 16
      , (maindrum =:= mainpad) & forBeats 32
      ]

-- Adds a drop to the given segment.
-- TODO: Avoid the explicit length passing
withDrop :: (Sigs a) => Sig -> Sig -> Seg a -> Seg a -> Seg a
withDrop len delay drop seg = newSeg =:= newDrop
  where
    newSeg = limSig (Beats gBPM delay) seg +:+ restSig (Beats gBPM len)
    newDrop = restSig (Beats gBPM delay) +:+ limSig (Beats gBPM len) drop

sat = runB gBPM song
