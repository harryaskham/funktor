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
numBeats = 2048

-- Compile with random dropout
compile :: DrumTab -> IO (SE Sig2)
compile tab = do
  g <- getStdGen
  return $ compileTabsDropOut bpm (dropOut g 0.2) (pure tab)

chh = compile $ DrumTab "o . . .|" Hm.chh numBeats
ohh = compile $ DrumTab "_ _ _ O|" Hm.ohh numBeats
bd2 = compile $ DrumTab "X _ _ _ _ _ _ _|o _ _ _ _ _ _ _|o _ _ _ _ _ _ _|o _ _ _ _ _ _ _" Hm.bd2 numBeats
clp = compile $ DrumTab "o o o o|_ _ _ _|_ _ _ _|_ _ _ _|_ _ _ _|_ _ _ _|_ _ _ _|_ _ _ _" Hm.clap numBeats

-- TODO: Need to find clean way to do this.
ttrigger :: Delayable a => a -> Int -> Int -> [DelayedSegment]
ttrigger = xEveryYBeatsForZBeats numBeats

song' :: IO Song
song' = do
  bd2' <- bd2
  chh' <- chh
  ohh' <- ohh
  clp' <- clp
  return $ Song bpm [ DelayedDrums bd2' (SegDelay 0) (SegDuration $ toSig numBeats)
                    , DelayedDrums chh' (SegDelay $ bars 4) (SegDuration $ toSig numBeats)
                    , DelayedDrums ohh' (SegDelay $ bars 8) (SegDuration $ toSig numBeats)
                    , DelayedDrums clp' (SegDelay $ bars 16) (SegDuration $ toSig numBeats) ]

song :: IO (SE Sig2)
song = compileSong <$> song'

rs :: IO ()
rs = runSong =<< song'
