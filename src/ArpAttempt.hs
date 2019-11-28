module ArpAttempt where

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
import Data.Sort
import Data.Ord

bpm = 140
numNotes = 256

arps root = [ sort $ expandScale [6, 7, 8, 9] (minorChord root) ?? 0.3 ?? 0.5
            , sort $ expandScale [8, 9, 10] (minorChord (doN 5 succC root)) ?? 0.3 ?? 0.75
            , sortOn Down $ expandScale [5, 6] (minorChord (doN 10 succC root)) <*> [0.3, 0.4] ?? 0.25
            , sortOn Down $ expandScale [8, 10] (minorScale root) <*> [0.3, 0.2] ?? 1
            ]

song' :: Note -> Song
song' root = Song bpm $ getZipList $ EnvSegment <$> segs <*> envs
  where
    segs = ZipList $ Segment bpm epiano1 <$> (toMel . take numNotes . cycle <$> arps root)
    envs = ZipList [ sinEnv bpm 0 (bars 1)
                   , sinEnv bpm 0.2 (bars 2)
                   , sinEnv bpm 0.4 (bars 3)
                   , sqrEnv bpm 0.6 (bars 4) ]

song :: SE Sig2
song = compileSong $ song' Fs

ras :: IO ()
ras = runB bpm song
