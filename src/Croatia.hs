module Croatia where

import Csound.Base hiding (Tab, clp, trigger)
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

bpm = 120

numBeats :: Int
numBeats = 512

compile = compileTabs bpm . pure

bd2 = compile $ DrumTab "O _ _ _|o _ _ _|o _ _ _|o _ _ _" Hm.bd2 numBeats
sn2 = compile $ DrumTab "_ _ _ o|_ _ o _|" Hm.sn2 numBeats
chh = compile $ DrumTab "o . . .|" Hm.chh numBeats
ohh = compile $ DrumTab "_ . o O|" Hm.ohh numBeats
clp = compile $ DrumTab "o o o o|" Hm.clap numBeats

chord = Segment bpm nightPad
  $ toChord
  $ Pch <$> [D, F, A] <*> pure 6 <*> pure 1.0 <*> pure (bars 2)

bell = Segment bpm tubularBell $ toMel $ replicate 8 $ Pch D 7 1.0 1

cello = Segment bpm celloSynt notes
  where
    notes = toMel [ Pch D 8 0.8 (bars 2)
                  , Pch A 7 0.8 (bars 1)
                  , Pch D 8 0.8 (bars 1) ]

trigger :: Delayable a => a -> Int -> Int -> [DelayedSegment]
trigger = xEveryYBeatsForZBeats numBeats

bassdrum = trigger bd2 32 28
hats = trigger chh 24 16
snares = trigger sn2 24 8
ohats = trigger ohh 32 4
claps = trigger clp 64 4

chords = trigger chord 16 8
cellos = trigger cello 96 16
bells = trigger bell 24 8

song' :: Song
song' = Song bpm segments
  where
    segments = chords
               ++ cellos
               ++ bells
               ++ bassdrum
               ++ snares
               ++ hats
               ++ ohats
               ++ claps
song :: SE Sig2
song = compileSong song'

-- rs = runSong song'
