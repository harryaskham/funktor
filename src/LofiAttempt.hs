module LofiAttempt where

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

bpm = 90

numBeats :: Int
numBeats = 128

compile :: DrumTab -> IO (SE Sig2)
compile tab = do
  g <- getStdGen
  return $ smallRoom2 <$> compileTabsDropOut bpm (dropOut g 0.0) (pure tab)

bd2 = compile $ DrumTab "X _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|X _ _ _ _ _ _ _|_ _ _ _ _ _ _ _" Hm.bd2 numBeats
sn1 = compile $ DrumTab "_ _ _ _ _ _ _ _|o _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|o _ _ _ _ _ _ _" Hm.sn1 numBeats

weights = zip [C, Eb, F, Fs, G, Bb] [3, 2, 2, 1, 3, 2] 
notes = weightsToPchs weights 

piano = do
  g <- getStdGen
  return $ Segment bpm epiano2 $ toMel $ rndNotes g 1024 $ notes <*> [7, 8, 8, 8, 9] <*> [0.5, 0.8, 0.85, 0.9] <*> [1, 1, 1/8, 1/4, 1/4, 1/2, 2]

song' :: IO Song
song' = do
  bd2' <- bd2
  sn1' <- sn1
  piano' <- piano
  return $ Song bpm [ DelayedDrums bd2' (SegDelay 0) (SegDuration $ toSig numBeats)
                    , DelayedDrums sn1' (SegDelay 0) (SegDuration $ toSig numBeats)
                    , DelayedSegment piano' (SegDelay 0) (SegDuration $ toSig numBeats)
                    ]

song :: IO (SE Sig2)
song = compileSong <$> song'

rs :: IO ()
rs = runSong =<< song'

rsm :: IO ()
rsm = runSongMono =<< song'
