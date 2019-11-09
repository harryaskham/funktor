module LofiAttempt where

import Csound.Base hiding (Tab, clp)
import qualified Csound.Catalog.Drum.Tr808 as Tr808
import qualified Csound.Catalog.Drum.Hm as Hm
import Csound.Patch
import Csound.Sam
import Csound.Sam.Core
import Data.List.Split
import Tabs
import Tools
import Note
import Melody
import Data.Functor ((<&>))
import System.Random
import Control.Monad

bpm = 90

numBeats :: Int
numBeats = 1024

compile :: DrumTab -> IO (SE Sig2)
compile tab = do
  g <- newStdGen
  return $ compileTabsDropOut bpm (dropOut g 0.1) (pure tab)

bd1 = compile $ DrumTab "O _ _ _ _ _ _ _|_ _ _ _ _ _ o _|_ _ _ _ _ _ _ _|_ _ o _ _ _ _ _" Hm.bd1 numBeats
sn1 = compile $ DrumTab "_ _ _ _ _ _ _ _|o _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|o _ _ _ _ _ _ _" Hm.sn1 numBeats
chh = compile $ DrumTab "_ _ _ _ _ _ o _|_ _ _ _ o _ _ _|o _ _ _ o _ o _|_ _ _ _ X _ _ _" Hm.chh numBeats

weights = [5, 3, 3, 2, 3, 3, 2] 
notesGb = weightsToPchs $ zip (minorScale Gb) weights 
notesCs = weightsToPchs $ zip (minorScale Cs) weights

chordsGb = [minorChord Gb, majorChord A, minorChord B, minorChord Cs, majorChord D, majorChord E]

chords = do
  g <- newStdGen
  return
    $ Segment bpm epiano1
    $ mel . fmap makeChord
    $ rndFrom g numBeats chordsGb
  where
    makeChord ch = toChord $ Pch <$> ch <*> [7] ?? 0.8 ?? bars 1

lead notes = do
  g <- newStdGen
  return
    $ Segment bpm banyan
    $ toMel $ rndFrom g numBeats 
    $ notes
    <*> [7, 7, 8, 8, 8, 9, 9]
    <*> [0.8, 0.85, 0.9] ++ replicate 3 0.0
    -- <*> [1, 1, 1/8, 1/4, 1/4, 1/2, 2]
    <*> [1]

motif notes = do
  g <- newStdGen
  return
    $ Segment bpm epiano1
    $ toMel . getZipList
    $ ZipList (rndFrom g (numBeats `div` 7) noteGen)
    <*> ZipList (cycle [4, 4, 1, 1, 1, 0.5, 12.5])
  where
    noteGen = notes
      <*> [7, 7, 8, 8, 8, 9, 9]
      <*> [0.8, 0.85, 0.9]

song' :: IO Song
song' = Song bpm <$> sequenceA segments
  where
    segments =
      [ DelayedDrums <$> bd1 ?? SegDelay 0 ?? (SegDuration $ toSig numBeats)
      , DelayedDrums <$> sn1 ?? SegDelay 0 ?? (SegDuration $ toSig numBeats)
      , DelayedDrums <$> chh ?? SegDelay 0 ?? (SegDuration $ toSig numBeats)
      , DelayedSegment <$> lead notesGb ?? SegDelay 0 ?? (SegDuration $ toSig numBeats)
      , DelayedSegment <$> motif notesGb ?? SegDelay 0 ?? (SegDuration $ toSig numBeats)
      , DelayedSegment <$> chords ?? SegDelay 0 ?? (SegDuration $ toSig numBeats)
      ]

song :: IO (SE Sig2)
song = compileSong <$> song'

rs :: IO ()
rs = runSong =<< song'
