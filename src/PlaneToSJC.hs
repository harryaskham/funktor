module PlaneToSJC where

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

bpm = 120

numBeats :: Int
numBeats = 1024

-- Compile with random dropout
compile :: DrumTab -> IO (SE Sig2)
compile tab = do
  g <- getStdGen
  return $ compileTabsDropOut bpm (dropOut g 0.3) (pure tab)

bd2 = compile $ DrumTab "X _ _ _ _ _ _ _|o _ _ _ _ _ _ _|o _ _ _ _ _ _ _|o _ _ _ _ _ _ _" Hm.bd2 numBeats
sn2 = compile $ DrumTab "_ _ _ _ _ _ O _|_ _ _ _ o _ . _|_ _ . _ . _ . _|_ _ _ _ _ _ O _" Hm.sn2 numBeats
chh = compile $ DrumTab "o . . ." Hm.chh numBeats
ohh = compile $ DrumTab "_ _ _ O" Hm.ohh numBeats
clp = compile $ DrumTab "o o o o|_ _ _ _|_ _ _ _|_ _ _ _|_ _ _ _|_ _ _ _|_ _ _ _|_ _ _ _" Hm.clap numBeats

chords = Segment bpm nightPad $ loopBy 128 . mel $ toChord <$>
  [ [ Pch C 8 1.0 (bars 2)
    , Pch Eb 8 1.0 (bars 2)
    , Pch G 8 1.0 (bars 2)
    ]
  , [ Pch F 8 1.0 (bars 2)
    , Pch Ab 8 1.0 (bars 2)
    , Pch C 9 1.0 (bars 2)
    ]
  , [ Pch G 8 1.0 (bars 4)
    , Pch Bb 8 1.0 (bars 4)
    , Pch D 9 1.0 (bars 4)
    ]
  ]

weights :: [(Note, Int)]
weights = [ (C, 10)
          , (Eb, 9)
          , (F, 7)
          , (G, 5)
          , (Bb, 6)
          , (Cs, 1)
          , (Fs, 1)
          ]
notes = weightsToPchs weights

bells = do
  g <- getStdGen
  return $ Segment bpm tubularBell $ toMel $ rndNotes g 1024 $ notes <*> [7, 8, 9] <*> [0.5, 0.8, 0.9] <*> [1, 2, 4, 8, 0]

lead = do
  g <- getStdGen
  return $ Segment bpm razorLead $ toMel $ rndNotes g 1024 $ notes <*> [8, 9] <*> [0.5] <*> [0.5, 1, 2] ++ replicate 17 0.0

bass = do
  g <- getStdGen
  return $ Segment bpm simpleBass $ toMel $ rndNotes g 1024 $ notes <*> [7] <*> [0.6] <*> [4, 0, 0, 0]

song' :: IO Song
song' = do
  bd2' <- bd2
  sn2' <- sn2
  chh' <- chh
  ohh' <- ohh
  clp' <- clp
  bells' <- bells
  lead' <- lead
  bass' <- bass
  return $ Song bpm [ DelayedDrums bd2' (SegDelay 0) (SegDuration $ toSig numBeats)
                    , DelayedDrums sn2' (SegDelay 4) (SegDuration $ toSig numBeats)
                    , DelayedDrums chh' (SegDelay 8) (SegDuration $ toSig numBeats)
                    , DelayedDrums ohh' (SegDelay 12) (SegDuration $ toSig numBeats)
                    , DelayedDrums clp' (SegDelay 16) (SegDuration $ toSig numBeats)
                    , DelayedSegment lead' (SegDelay 16) (SegDuration $ toSig numBeats)
                    , DelayedSegment bass' (SegDelay 0) (SegDuration $ toSig numBeats)
                    , DelayedSegment bells' (SegDelay 8) (SegDuration $ toSig numBeats)
                    , DelayedSegment chords (SegDelay 0) (SegDuration $ toSig numBeats)
                    ]

song :: IO (SE Sig2)
song = compileSong <$> song'
