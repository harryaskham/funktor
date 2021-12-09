{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}

module Sketches where

import Control.Lens
import Control.Monad.Random
import Control.Monad.Reader
import Csound.Base hiding (Duration, Tab, clp)
import qualified Csound.Catalog.Drum.Hm as Hm
import Csound.Catalog.Drum.Tr808 hiding (bass)
import Csound.Catalog.Effect
import Csound.Patch
import Csound.Sam
import Data.List
import Data.Ord
import Data.Sort
import Melody
import Note
import System.IO.Unsafe
import System.Random
import Tabs
import Tools hiding (bpm)

bpm = 128

songEnv = SongEnv bpm 128

n octave note = Pch note octave 0.5 1

play s = dac =<< runSongM songEnv (har <$> s)

record s = runToDisk =<< runSongM songEnv (har <$> s)

piano = compileI epianoBright

bass = compileI (withDeepBass 0.75 pwBass)

zipEnvs es xs' =
  do
    xs <- sequence xs'
    return [withEnv e x | (e, x) <- zip es xs]

sketch1 :: IO ()
sketch1 = record do
  es <-
    sequence $
      sqrEnvM
        <$+> [0.5, 1 / 2, 1 / 4, 3 / 4, 1 / 8, 0, 1 / 2]
        <*++> [16, 24, 12, 32, 16, 1 / 4, 4]
  zipEnvs
    es
    [ drums "X|o|o|o|" bd2,
      drums "_|X" sn,
      drums "O o _ .|" chh,
      drums "_ _ X _|" ohh,
      drums "_|_|_|. o O X|" cym,
      (piano $ n <$> [7, 8] <*> minorChord Eb) <**> (withEnv <$> sqrEnvM (1 / 5) 4),
      bass $ n 6 <$> (take 5 . cycle . reverse . minorChord =<< [Ab, Eb])
    ]
