module ArpAttempt where

import Csound.Base hiding (Tab, clp, Duration)
import qualified Csound.Catalog.Drum.Tr808 as Tr808
import qualified Csound.Catalog.Drum.Hm as Hm
import qualified Csound.Catalog.Drum as Drum
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
numBeats = 256

organ :: Note -> IO TrackSegment
organ root = do
  notes <- noteCycle numBeats 8 root [8] [0.3] (repeat 1)
  return $ Segment bpm epiano2 $ toMel notes

highs :: Note -> IO TrackSegment
highs root = do
  notes <- noteCycle numBeats 10 root [8] [0.9] (cycle [1, 15])
  return $ Segment bpm epiano2 $ toMel notes

song' :: Note -> IO Song
song' root = Song bpm <$> sequenceA instrSegments
  where
    instrSegments = genEnvSegs [organ, highs] root constEnv

song :: IO (SE Sig2)
song = compileSong <$> song' Fs

rs :: IO ()
rs = runB bpm =<< song
