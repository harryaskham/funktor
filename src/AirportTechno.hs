module AirportTechno where

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

bpm = 160

numBeats :: Int
numBeats = 256

compile :: DrumTab -> IO (SE Sig2)
compile tab = do
  g <- newStdGen
  return $ compileTabsDropOut bpm (dropOut g 0.0) (pure tab)

bd1 = compile $ DrumTab "X _ _ _ _ _ _ _|O _ _ _ _ _ _ _|O _ _ _ _ _ _ _|O _ _ _ _ _ _ _" Hm.bd1 numBeats
sn1 = compile $ DrumTab "_ _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|_ _ _ _ _ _ _ _" Hm.sn1 numBeats
chh = compile $ DrumTab "_ _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|_ _ _ _ _ _ _ _" Hm.chh numBeats

song' :: Note -> IO Song
song' root = Song bpm <$> sequenceA drumSegments
  where
    drumSegments =
      [ EnvDrums <$> bd1 ?? constEnv
      , EnvDrums <$> sn1 ?? constEnv
      , EnvDrums <$> chh ?? constEnv ]

-- Procedurally generated lofi
song :: IO (SE Sig2)
song = compileSong <$> song' Fs

rs :: IO ()
rs = runB bpm =<< song
