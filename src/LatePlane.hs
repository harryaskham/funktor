module LatePlane where

import Csound.Base hiding (Tab, clp, Duration)
import Csound.Patch
import Tabs
import Tools
import Note
import Melody
import Data.Sort
import Data.Ord

bpm = 80
numBeats = 512

arps root = toMel . repeatToBeats numBeats <$>
  [ [Pch root 6 0.5 1]
  , sort $ expandScale [9, 9] (minorChord root) ?? 0.4 ?? 0.75
  , expandScale [5, 8] (minorScale root) ?? 0.4 ?? 0.25
  , expandScale [4, 5] (minorChord (doN 3 succC root)) <*> [0.2, 0.4] ?? 0.5
  ]

envs = repeat constEnv

song' :: Note -> Song
song' root = Song bpm $ EnvSegment <$+> segs <*++> envs
  where
    segs = Segment bpm guitar <$> arps root

song :: SE Sig2
song = compileSong $ song' A

rps :: IO ()
rps = runB bpm song
