module AirportTechno where

import Csound.Base hiding (Tab, clp, Duration)
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

bpm = 160

numBeats :: Int
numBeats = 256

compile = compileWithDropOut 0.0 bpm
bd2 = compile $ DrumTab "X _ _ _ _ _ _ _|O _ _ _ _ _ _ _|O _ _ _ _ _ _ _|O _ _ _ _ _ _ _" Hm.bd2 numBeats
sn1 = compile $ DrumTab "_ _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|_ _ _ _ X _ _ _" Hm.sn1 numBeats
chh = compile $ DrumTab "O _ o _ O _ o _|_ _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|_ _ _ _ _ _ _ _" Hm.chh numBeats

razor :: Note -> IO TrackSegment
razor root = do
  notes <- noteCycle numBeats 16 C [8] [0.5] (repeat 0.5)
  return $ Segment bpm razorLead $ toMel notes

song' :: Note -> IO Song
song' root = Song bpm <$> sequenceA (drumSegments ++ instrSegments)
  where
    drumSegments =
      [ EnvDrums <$> bd2 ?? constEnv
      , EnvDrums <$> sn1 ?? constEnv
      , EnvDrums <$> chh ?? constEnv ]
    instrSegments = genEnvSegs [razor] root constEnv

-- Procedurally generated lofi
song :: IO (SE Sig2)
song = compileSong <$> song' Fs

rs :: IO ()
rs = runB bpm =<< song
