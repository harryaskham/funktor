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

bpm = 120

numBeats :: Int
numBeats = 256

arpNotes :: Note -> [Pch]
arpNotes root = take 250 . cycle . sort $ expandScale [6, 7, 8, 9] (minorChord root) ?? 0.5 ?? 0.25

arp1 :: Note -> TrackSegment
arp1 root = Segment bpm epiano2 $ toMel (arpNotes root)

song' :: Note -> Song
song' root = Song bpm [instrSegment]
  where
    instrSegment = EnvSegment (arp1 root) constEnv

song :: SE Sig2
song = compileSong $ song' Fs

ras :: IO ()
ras = runB bpm song
