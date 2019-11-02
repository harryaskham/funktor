module NewSong where

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

bpm = 120

numBeats :: Int
numBeats = 512
numBeatsSig = toSig numBeats

drumsFx = fmap largeHall2
compile = drumsFx . compileTabs bpm . pure
bd2 = compile $ DrumTab "O _ _ _|o _ _ _|o _ _ _|o _ _ _" Hm.bd2
sn2 = compile $ DrumTab "_ _ _ o|_ _ o _|" Hm.sn2
chh = compile $ DrumTab "o . . .|" Hm.chh
ohh = compile $ DrumTab "_ . o O|" Hm.ohh
clp = compile $ DrumTab "o o o o|" Hm.clap

chord = Segment bpm nightPad looped
  where
    chord1 = toChord $ Pch <$> [D, F, A] <*> pure 6 <*> pure 1.0 <*> pure (bars 2)
    gap = toMel [ Silent (bars 2) ]
    looped = loop . mel $ [chord1, gap]

-- Play x drum every y beats for z beats duration
xEveryYBeatsForZBeats :: SE Sig2 -> Int -> Sig -> [DelayedSegment]
xEveryYBeatsForZBeats x y z = makeDrum <$> delays
  where
    makeDrum d = DelayedDrums x (SegDelay d) (SegDuration z)
    delays = toSig <$> [y, y*2 .. numBeats]

-- Make some delayed beats
bassdrum = xEveryYBeatsForZBeats bd2 32 28
hats = xEveryYBeatsForZBeats chh 24 16
snares = xEveryYBeatsForZBeats sn2 16 12
ohats = xEveryYBeatsForZBeats ohh 32 4
claps = xEveryYBeatsForZBeats clp 64 4

song' :: Song
song' = Song bpm segments
  where
    segments = [ DelayedSegment chord (SegDelay 0) (SegDuration numBeatsSig) ]
               ++ bassdrum
               ++ snares
               ++ hats
               ++ ohats
               ++ claps
song :: SE Sig2
song = compileSong song'

rs = runSong song'
