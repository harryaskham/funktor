module ArpAttempt where

import Csound.Base hiding (Tab, clp, Duration)
import Csound.Patch
import Tabs
import Tools
import Note
import Melody
import Data.Sort
import Data.Ord
import Control.Lens

gBPM = 140
numBeats = 512

arps root = toMel . repeatToBeats numBeats <$>
  [ sort $ expandScale [6, 7, 8, 9] (minorChord root) ?? 0.3 ?? 0.5
  , sort $ expandScale [8, 9, 10] (minorChord (doN 5 succC root)) ?? 0.3 ?? 0.75
  , sortOn Down $ expandScale [5, 6] (minorChord (doN 10 succC root)) <*> [0.3, 0.4] ?? 0.25
  , sortOn Down $ expandScale [8, 10] (minorScale root) <*> [0.3, 0.2] ?? 1
  , sort $ expandScale [8] (majorChord (doN 3 succC root)) ?? 0.7 ?? 3
  ] -- ++ [ loopBy 64 . mel $ toChord <$> ((((Pch <$$> minorChords root) ??? 8) <***> [0.5, 0.3]) ??? 1) ]

envs =
  [ modEnv $ sinEnv gBPM 0 (bars 1)
  , modEnv $ sinEnv gBPM 0.2 (bars 2)
  , modEnv $ sinEnv gBPM 0.4 (bars 3)
  , sqrEnv gBPM 0.6 (bars 4)
  , modEnv $ sinEnv gBPM 0.8 (bars 5)
  , (sqrEnv gBPM 0.5 (bars 4)) * (sqrEnv gBPM 0 1)
  ]

song' :: Note -> Song
song' root = Song gBPM $ EnvSegment <$+> segs <*++> envs
  where
    segs = Segment gBPM epiano1 <$> arps root

song :: SE Sig2
song = compileSong $ song' Fs

ras :: IO ()
ras = runB gBPM song
