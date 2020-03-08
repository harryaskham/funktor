module Series where

import Csound.Base hiding (Tab, clp, Duration)
import Csound.Sam
import Csound.Patch
import Tabs
import Tools
import Note
import Melody
import Data.Sort
import Data.Ord
import Control.Lens
import Csound.Catalog.Drum.Tr808 as Tr808
import Csound.Catalog.Drum.Hm as Hm
import Csound.Catalog.Effect
import System.IO.Unsafe
import Control.Monad.Reader
import Control.Monad.Random
import Data.List
import System.Random
import System.Random.Shuffle

fibSeries :: [Integer]
fibSeries = 1:1:fibFrom 1 1
  where
    fibFrom a b = c:fibFrom b c
      where
        c = a + b

cyclicGet :: [a] -> Int -> a
cyclicGet xs i = xs !! (i `mod` length xs)

song :: SongM
song = do
  let notes' = expandScale [6..9] (bluesScale C) <*> [0.5] <*> [1/2, 1/4, 1]
      notes = cyclicGet notes' <$> (fromInteger . (`mod` 128) <$> fibSeries)
  notes <- shuffleM (take 128 notes)
  compileI hammondOrgan notes

songEnv = SongEnv { _bpm=110
                  , _beatLength=128
                  }

srs' = runSongM songEnv song
srs = dac =<< srs'
