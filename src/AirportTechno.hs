module AirportTechno where

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
import Control.Lens

gBPM = 90

numBeats :: Int
numBeats = 256

compile = compileWithDropOut 0.2 gBPM
bd2 = compile $ DrumTab "X _ _ _ _ _ _ _|O _ _ _ _ _ _ _|O _ _ _ _ _ _ _|O _ _ _ _ _ _ _" Drum.mpBd
sn1 = compile $ DrumTab "_ _ _ _ _ _ o _|_ _ _ _ o _ _ _|_ _ _ _ _ _ o _|_ _ _ _ X _ _ _" Drum.mpSn1
sn2 = compile $ DrumTab "_ _ o _ _ _ _ _|_ _ _ _ _ _ . _|_ _ _ _ . _ _ _|_ _ _ _ X _ X _" Drum.mpSn2
cl  = compile $ DrumTab "O _ o _ O _ o _|_ _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|_ _ _ _ _ _ _ _" Drum.mpCl
bn1 = compile $ DrumTab "_ O _ _ O _ O _|" Drum.mpBon1
bn2 = compile $ DrumTab "O _ O _ _ O _ _|" Drum.mpBon2
bn3 = compile $ DrumTab "_ _ _ O _ _ _ O|" Drum.mpBon3

organ :: Note -> IO TrackSegment
organ root = do
  notes <- noteCycle numBeats 8 root [8] [0.3] (repeat 1)
  return $ Segment gBPM epiano2 $ toMel notes

highs :: Note -> IO TrackSegment
highs root = do
  notes <- noteCycle numBeats 10 root [8] [0.9] (cycle [1, 15])
  return $ Segment gBPM epiano2 $ toMel notes

song' :: Note -> IO Song
song' root = Song gBPM <$> sequenceA (drumSegments ++ instrSegments)
  where
    drumSegments = [
        EnvDrums <$> bd2 ?? constEnv
      , EnvDrums <$> sn1 ?? constEnv
      , EnvDrums <$> sn2 ?? constEnv 
      , EnvDrums <$> cl ?? constEnv
      , EnvDrums <$> bn1 ?? sqrEnv gBPM 0 (bars 2)
      , EnvDrums <$> bn2 ?? sqrEnv gBPM 0 (bars 4)
      , EnvDrums <$> bn3 ?? sqrEnv gBPM 0 (bars 8)
      ]
    instrSegments = genEnvSegs [organ, highs] root constEnv

song :: IO (SE Sig2)
song = compileSong <$> song' Fs

rs :: IO ()
rs = runB gBPM =<< song
