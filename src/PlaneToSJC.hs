module PlaneToSJC where

import Csound.Base hiding (Tab)
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
numBeats = 512

-- Compile with random dorpout
compile :: IO (DrumTab -> SE Sig2)
compile = do
  dropOut <- randomDropout <$> getStdGen
  return $ compileTabsDropOut bpm dropOut . pure

bd2 = compile <*> pure (DrumTab "O o o o o o o o|o o o o o o o o|o o o o o o o o|o o o o o o o o" Hm.bd2)

-- TODO: Need to find clean way to do this.
ttrigger :: Delayable a => a -> Int -> Int -> [DelayedSegment]
ttrigger = xEveryYBeatsForZBeats numBeats

song' :: IO Song
song' = do
  drums <- bd2
  return $ Song bpm $ ttrigger drums 16 16

song :: IO (SE Sig2)
song = compileSong <$> song'

rs :: IO ()
rs = runSong =<< song'
