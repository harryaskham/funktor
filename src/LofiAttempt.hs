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
numBeats = 512

compile :: DrumTab -> IO (SE Sig2)
compile tab = do
  g <- newStdGen
  return $ compileTabsDropOut bpm (dropOut g 0.1) (pure tab)

bd1 = compile $ DrumTab "O _ _ _ _ _ _ _|_ _ _ _ _ _ o _|_ _ _ _ _ _ _ _|_ _ o _ _ _ _ _" Hm.bd1 numBeats
sn1 = compile $ DrumTab "_ _ _ _ _ _ _ _|o _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|o _ _ _ _ _ _ _" Hm.sn1 numBeats
chh = compile $ DrumTab "_ _ _ _ _ _ o _|_ _ _ _ o _ _ _|o _ _ _ o _ o _|_ _ _ _ X _ _ _" Hm.chh numBeats

-- Weights for picking notes out of the scale
weights = [5, 3, 3, 2, 3, 3, 2] 

chords :: Note -> IO TrackSegment
chords root = do
  g <- newStdGen
  return
    $ Segment bpm epiano1
    $ mel . fmap makeChord
    $ rndFrom g numBeats (minorChords root)
  where
    makeChord ch = toChord $ Pch <$> ch <*> [7] ?? 0.8 ?? bars 1

lead :: Note -> IO TrackSegment
lead root = do
  g <- newStdGen
  return
    $ Segment bpm banyan
    $ toMel $ rndFrom g numBeats 
    $ notes
    <*> [7, 7, 8, 8, 8, 9, 9]
    <*> [0.8, 0.85, 0.9] ++ replicate 3 0.0
    -- <*> [1, 1, 1/8, 1/4, 1/4, 1/2, 2]
    <*> [1]
  where
    notes = weightsToPchs $ zip (minorScale root) weights

motif :: Note -> IO TrackSegment
motif root = do
  g <- newStdGen
  return
    $ Segment bpm epiano1
    $ toMel . getZipList
    $ ZipList (rndFrom g numBeats noteGen)
    <*> ZipList (cycle [4, 4, 1, 1, 1, 0.5, 12.5])
  where
    notes = weightsToPchs $ zip (minorScale root) weights
    noteGen = notes
      <*> [7, 7, 8, 8, 8, 9, 9]
      <*> [0.8, 0.85, 0.9]

-- Take some instrument gens and create the corresponding verse.
makeSegs :: [Note -> IO TrackSegment] -> Note -> Int -> Int -> [IO DelayedSegment]
makeSegs instrs root del dur = DelayedSegment <$$> (instrs ?? root) ??? SegDelay (toSig del) ??? SegDuration (toSig dur)

instrSegments = join $ getZipList
  $ makeSegs [chords, lead, motif]
  <$> ZipList (cycle [Gb, C])
  <*> ZipList [0,16..128]
  <*> ZipList (repeat 16)

testSegs = sequenceA $ do
  segs <- instrSegments
  return $ compileDelayedSegment 90 <$> segs

song' :: IO Song
song' = Song bpm <$> sequenceA (drumSegments ++ instrSegments)
  where
    drumSegments =
      [ DelayedDrums <$> bd1 ?? SegDelay 0 ?? (SegDuration $ toSig numBeats)
      , DelayedDrums <$> sn1 ?? SegDelay 0 ?? (SegDuration $ toSig numBeats)
      , DelayedDrums <$> chh ?? SegDelay 0 ?? (SegDuration $ toSig numBeats) ]
      -- TODO: Something wrong with above, isn't respecting the delay or duration properly.
      -- As soon as we sum segments together the delays get lost.

song :: IO (SE Sig2)
song = compileSong <$> song'

rs :: IO ()
rs = runSong =<< song'
