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
numBeats = 512

arps root = ZipList $ toMel . repeatToBeats numBeats <$>
  [ sort $ expandScale [6, 7, 8, 9] (minorChord root) ?? 0.3 ?? 0.5
  , sort $ expandScale [8, 9, 10] (minorChord (doN 5 succC root)) ?? 0.3 ?? 0.75
  , sortOn Down $ expandScale [5, 6] (minorChord (doN 10 succC root)) <*> [0.3, 0.4] ?? 0.25
  , sortOn Down $ expandScale [8, 10] (minorScale root) <*> [0.3, 0.2] ?? 1
  , sort $ expandScale [8] (majorChord (doN 3 succC root)) ?? 0.7 ?? 3
  ]

envs = ZipList
  [ modEnv $ sinEnv bpm 0 (bars 1)
  , modEnv $ sinEnv bpm 0.2 (bars 2)
  , modEnv $ sinEnv bpm 0.4 (bars 3)
  , sqrEnv bpm 0.6 (bars 4)
  , modEnv $ sinEnv bpm 0.8 (bars 5)
  ]

modEnv :: SegEnv -> SegEnv
modEnv (SegEnv s) = SegEnv $ (s * (1.0 - balance)) + (balance * s)
  where
    balance = 0.2

song' :: Note -> Song
song' root = Song bpm $ getZipList $ EnvSegment <$> segs <*> envs
  where
    segs = Segment bpm epiano1 <$> arps root

song :: SE Sig2
song = compileSong $ song' Fs

ras :: IO ()
ras = runB bpm song
