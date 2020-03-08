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

fibSeries :: [Integer]
fibSeries = 1:1:fibFrom 1 1
  where
    fibFrom a b = c:fibFrom b c
      where
        c = a + b

cyclicGet :: [a] -> Int -> a
cyclicGet xs i = xs !! (i `mod` length xs)

notes' = sort $ expandScale [4..9] (minorScale C) <*> [0.5] <*> [1, 2, 4]
notes = cyclicGet notes' <$> (fromInteger <$> fibSeries)
song = compileI razorPadSlow notes

songEnv = SongEnv { _bpm=110
                  , _beatLength=128
                  }

srs' = runSongM songEnv song
srs = dac =<< srs'
