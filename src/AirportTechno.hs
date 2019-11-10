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

bpm = 90

numBeats :: Int
numBeats = 256

compile = compileWithDropOut 0.2 bpm
bd2 = compile $ DrumTab "X _ _ _ _ _ _ _|O _ _ _ _ _ _ _|O _ _ _ _ _ _ _|O _ _ _ _ _ _ _" Drum.mpBd numBeats
sn1 = compile $ DrumTab "_ _ _ _ _ _ o _|_ _ _ _ o _ _ _|_ _ _ _ _ _ o _|_ _ _ _ X _ _ _" Drum.mpSn1 numBeats
sn2 = compile $ DrumTab "_ _ o _ _ _ _ _|_ _ _ _ _ _ . _|_ _ _ _ . _ _ _|_ _ _ _ X _ X _" Drum.mpSn2 numBeats
cl  = compile $ DrumTab "O _ o _ O _ o _|_ _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|_ _ _ _ _ _ _ _" Drum.mpCl numBeats
bn1 = compile $ DrumTab "_ O _ _ O _ O _|" Drum.mpBon1 numBeats
bn2 = compile $ DrumTab "O _ O _ _ O _ _|" Drum.mpBon2 numBeats
bn3 = compile $ DrumTab "_ _ _ O _ _ _ O|" Drum.mpBon3 numBeats

pad :: Note -> IO TrackSegment
pad root = do
  notes <- noteCycle numBeats 3 Cs [7] [0.3] (cycle [4, 4, 8])
  return $ Segment bpm lightIsTooBrightPad $ toMel notes

song' :: Note -> IO Song
song' root = Song bpm <$> sequenceA (drumSegments ++ instrSegments)
  where
    drumSegments = [
        EnvDrums <$> bd2 ?? constEnv
      , EnvDrums <$> sn1 ?? constEnv
      , EnvDrums <$> sn2 ?? constEnv 
      , EnvDrums <$> cl ?? constEnv
      , EnvDrums <$> bn1 ?? sqrEnv bpm 0 (bars 2)
      , EnvDrums <$> bn2 ?? sqrEnv bpm 0 (bars 4)
      , EnvDrums <$> bn3 ?? sqrEnv bpm 0 (bars 8)
      ]
    instrSegments = genEnvSegs [pad] root constEnv

-- Procedurally generated lofi
song :: IO (SE Sig2)
song = compileSong <$> song' Fs

rs :: IO ()
rs = runB bpm =<< song
