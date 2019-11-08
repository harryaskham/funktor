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

bpm = 140

numBeats :: Int
numBeats = 128

compile :: DrumTab -> IO (SE Sig2)
compile tab = do
  g <- getStdGen
  return $ smallRoom2 <$> compileTabsDropOut bpm (dropOut g 0.0) (pure tab)

bd2 = compile $ DrumTab "X _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|X _ _ _ _ _ _ _|_ _ _ _ _ _ _ _" Hm.bd2 numBeats
sn1 = compile $ DrumTab "_ _ _ _ _ _ _ _|o _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|o _ _ _ _ _ _ _" Hm.sn1 numBeats
chh = compile $ DrumTab "o _ _ _ _ _ . _|_ _ _ _ _ _ _ _|" Hm.chh numBeats
ohh = compile $ DrumTab "_ _ _ _ _ _ _ _|o _ _ _ _ _ _ _|" Hm.cr numBeats

chords = Segment bpm nightPad $ loopBy 128 . mel $ toChord <$>
  [ [ Pch C 7 1.0 (bars 2)
    , Pch E 7 1.0 (bars 2)
    , Pch G 7 1.0 (bars 2)
    ]
  , [ Pch F 7 1.0 (bars 2)
    , Pch A 7 1.0 (bars 2)
    , Pch C 8 1.0 (bars 2)
    ]
  , [ Pch G 8 1.0 (bars 1)
    , Pch B 8 1.0 (bars 1)
    , Pch D 9 1.0 (bars 1)
    ]
  , [ Pch F 7 1.0 (bars 1)
    , Pch A 7 1.0 (bars 1)
    , Pch C 8 1.0 (bars 1)
    ]
  , [ Pch C 7 1.0 (bars 2)
    , Pch E 7 1.0 (bars 2)
    , Pch G 7 1.0 (bars 2)
    ]
  ]

weights = zip [C, Eb, F, Fs, G, Bb] [5, 3, 3, 1, 3, 3] 
notes = weightsToPchs weights 

lead = do
  g <- getStdGen
  return
    $ Segment bpm razorLead
    $ toMel $ rndNotes g 1024 
    $ notes
    <*> [7, 7, 8, 8, 8, 9, 9]
    <*> [0.8, 0.85, 0.9]
    <*> [1, 1, 1/8, 1/4, 1/4, 1/2, 2] ++ replicate 4 0

motif = do
  g <- getStdGen
  return
    $ Segment bpm largeTibetanBowl180
    $ loopBy 32 . toMel
    $ rndNotes g 8
    $ notes
    <*> [7, 7, 8, 8, 8, 9, 9]
    <*> [0.8, 0.85, 0.9]
    <*> [1, 1, 1, 1/8, 1/4, 1/2, 2, 2, 2, 4] ++ replicate 5 0

song' :: IO Song
song' = Song bpm <$> sequenceA segments
  where
    segments =
      [ DelayedDrums <$> bd2 ?? SegDelay 0 ?? (SegDuration $ toSig numBeats)
      , DelayedDrums <$> sn1 ?? SegDelay 0 ?? (SegDuration $ toSig numBeats)
      , DelayedDrums <$> chh ?? SegDelay 0 ?? (SegDuration $ toSig numBeats)
      , DelayedDrums <$> ohh ?? SegDelay 0 ?? (SegDuration $ toSig numBeats)
      , DelayedSegment <$> lead ?? SegDelay 0 ?? (SegDuration $ toSig numBeats)
      , DelayedSegment <$> motif ?? SegDelay 0 ?? (SegDuration $ toSig numBeats)
      , pure $ DelayedSegment chords (SegDelay 0) (SegDuration $ toSig numBeats)
      ]

song :: IO (SE Sig2)
song = compileSong <$> song'

rs :: IO ()
rs = runSong =<< song'
