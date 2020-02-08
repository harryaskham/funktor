module AuldLangSyne where

import Csound.Base hiding (Tab, clp, Duration)
import Csound.Patch
import Tabs
import Tools
import Note
import Melody
import Data.Sort
import Data.Ord
import Control.Lens

bpm = 180
numBeats = 512

compileChord :: [Note] -> Octave -> Velocity -> Duration -> [Pch]
compileChord cs oct vel dur = Pch <$> cs ?? oct ?? vel ?? dur

chords :: Note -> TrackSegment
chords root = Segment bpm cathedralOrgan compiled
  where
    notes = ZipList $ doN <$> [0, 7, 0, 5, 0, 7, 9, 5, 0] <*> pure succC <*> pure root
    tonics = ZipList [majorChord, majorChord, majorChord, majorChord, majorChord, majorChord, minorChord, majorChord, majorChord]
    durs = [4, 4, 4, 4, 4, 4, 3, 1, 4]
    chordNotes = getZipList (tonics <*> notes)
    chords' = ZipList (compileChord <$> chordNotes <*> pure 6 <*> pure 0.5) <*> ZipList durs
    compiled = loopBy 32 $ mel $ toChord <$> getZipList chords'

song' :: Note -> Song
song' root = Song bpm [EnvSegment (chords root) constEnv]

song :: SE Sig2
song = compileSong $ song' C

als :: IO ()
als = runB bpm song
