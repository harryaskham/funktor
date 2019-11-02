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
numBeats = 2048
numBeatsSig = toSig numBeats

-- drumsFx = fmap smallHall2
drumsFx = id
compile = drumsFx . compileTabs bpm . pure

bd2 = compile $ DrumTab "O _ _ _|o _ _ _|o _ _ _|o _ _ _" Hm.bd2
sn2 = compile $ DrumTab "_ _ _ o|_ _ o _|" Hm.sn2
chh = compile $ DrumTab "o . . .|" Hm.chh
ohh = compile $ DrumTab "_ . o O|" Hm.ohh
clp = compile $ DrumTab "o o o o|" Hm.clap

chord = Segment bpm nightPad chord
  where
    chord = toChord $ Pch <$> [D, F, A] <*> pure 6 <*> pure 1.0 <*> pure (bars 2)

bell = Segment bpm tubularBell $ loopBy 2 $ toMel notes
  where
    notes = [ Pch F 6
            , Pch D 6
            , Pch A 6
            , Pch F 6 ] <*> pure 1.0 <*> pure 0.5

cello = Segment bpm celloSynt notes
  where
    notes = toMel [ Pch D 8 0.8 (bars 2)
                  , Pch A 7 0.8 (bars 1)
                  , Pch D 8 0.8 (bars 1) ]

-- Play x drum every y beats for z beats duration
xEveryYBeatsForZBeats :: SE Sig2 -> Int -> Sig -> [DelayedSegment]
xEveryYBeatsForZBeats x y z = makeDrum <$> delays
  where
    makeDrum d = DelayedDrums x (SegDelay d) (SegDuration z)
    delays = toSig <$> [y, y*2 .. numBeats]

-- TODO: Use typeclasses to make this better.
xSegmentEveryYBeatsForZBeats :: TrackSegment -> Int -> Sig -> [DelayedSegment]
xSegmentEveryYBeatsForZBeats x y z = makeDelayedSegment <$> delays
  where
    makeDelayedSegment d = DelayedSegment x (SegDelay d) (SegDuration z)
    delays = toSig <$> [y, y*2 .. numBeats]

bassdrum = xEveryYBeatsForZBeats bd2 32 28
hats = xEveryYBeatsForZBeats chh 24 16
snares = xEveryYBeatsForZBeats sn2 24 8
ohats = xEveryYBeatsForZBeats ohh 32 4
claps = xEveryYBeatsForZBeats clp 64 4

chords = xSegmentEveryYBeatsForZBeats chord 16 8
cellos = xSegmentEveryYBeatsForZBeats cello 96 16
bells = xSegmentEveryYBeatsForZBeats bell 24 8

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

rs = runSong song'
