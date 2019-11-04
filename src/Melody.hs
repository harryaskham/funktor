{-# LANGUAGE FlexibleInstances #-}

module Melody where

import Csound.Base
import Csound.Patch
import Csound.Sam
import Tools
import Note
import Data.Tuple.Extra

-- A convenience alias for compiled drums.
type Drums = SE Sig2

-- Definition of a song segment including BPM and instrument information.
data Segment a = Segment Bpm Patch2 a

-- Allow us to map over segments to create morphing track pieces.
instance Functor Segment where
  fmap f (Segment bpm patch a) = Segment bpm patch (f a)

-- Helper to run a segment by itself in isolation.
runSegment :: TrackSegment -> IO ()
runSegment seg@(Segment bpm patch notes) = runB bpm $ pure $ compileSegment seg

-- The type of segment we're using here.
type TrackSegment = Segment (Track Sig (D, D))

-- The delay of a segment in beats.
newtype SegDelay = SegDelay Sig

-- The duration of a segment in beats.
newtype SegDuration = SegDuration Sig

-- TODO: Delayable could be used here to reconcile the delayed segment
-- piece with class constraint on first data element?

-- A song represented as the parallel segments to play for the given duration
data DelayedSegment = DelayedSegment TrackSegment SegDelay SegDuration
                    | DelayedDrums Drums SegDelay SegDuration

-- A combination of delayed segments and drum information.
data Song = Song Bpm [DelayedSegment]

class Delayable a where
  -- Make a DelayedSegment from the given contents.
  make :: a -> Int -> Int -> DelayedSegment

instance Delayable (SE Sig2) where
  make d del dur = DelayedDrums d (SegDelay $ toSig del) (SegDuration $ toSig dur)

instance Delayable TrackSegment where
  make t del dur = DelayedSegment t (SegDelay $ toSig del) (SegDuration $ toSig dur)

-- Play x drum every y beats for z beats duration
xEveryYBeatsForZBeats :: Delayable a => Int -> a -> Int -> Int -> [DelayedSegment]
xEveryYBeatsForZBeats numBeats x y z = (\del -> make x del z) <$> [y, y*2 .. numBeats]

-- Compile the given segment as a Seg
compileToSeg :: TrackSegment -> Seg Sig2
compileToSeg = toSeg . compileSegment

-- Compiles the given segment to a signal
compileSegment :: TrackSegment -> Sig2
compileSegment (Segment bpm patch track) = mix . atSco patch . fmap cpspch2 . str (spb bpm) $ track

-- Introduces silence at the start of the segment
withDelay :: Sig -> TrackSegment -> TrackSegment
withDelay delay = fmap (mel . (toMel [Silent delay]:) . pure)

-- Compiles the given delayed segment to a track segment with its delay
compileDelayedSegment :: Bpm -> DelayedSegment -> SE Sig2
compileDelayedSegment bpm (DelayedSegment t (SegDelay del) (SegDuration dur)) = pure limited
  where
    delayed = withDelay del t
    compiled = compileSegment delayed
    limited = limSig (Beats bpm (del + dur)) compiled
compileDelayedSegment bpm (DelayedDrums drums (SegDelay del) (SegDuration dur)) = delayed
  where
    limited = limSig (Beats bpm dur) <$> drums
    delayed = delaySnd (beatsToSecs (Beats bpm del)) <$> limited

-- Compile the given delayed segments into their corresponding signal.
compileDelayedSegments :: Bpm -> [DelayedSegment] -> SE Sig2
compileDelayedSegments bpm = sum . fmap (compileDelayedSegment bpm)

-- Sets the delay of a segment
setDelay :: Sig -> DelayedSegment -> DelayedSegment
setDelay del (DelayedSegment t (SegDelay _) (SegDuration dur)) =
  DelayedSegment t (SegDelay del) (SegDuration dur)
setDelay del (DelayedDrums d (SegDelay _) (SegDuration dur)) =
  DelayedDrums d (SegDelay del) (SegDuration dur)

-- Removes the delays so that segments can be previewed all at once.
removeDelays :: [DelayedSegment] -> [DelayedSegment]
removeDelays = fmap withNoDelay
  where
    withNoDelay (DelayedSegment track _ dur) = DelayedSegment track (SegDelay 0) dur
    withNoDelay (DelayedDrums drums _ dur) = DelayedDrums drums (SegDelay 0) dur

-- Compiles a song down to its signal
compileSong :: Song -> SE Sig2
compileSong (Song bpm delayedSegments) = compileDelayedSegments bpm delayedSegments

-- Runs a song.
runSong :: Song -> IO ()
runSong song@(Song bpm delayedSegments) = runB bpm $ compileSong song

-- Hack while only one earphone works.
runSongMono :: Song -> IO ()
runSongMono song@(Song bpm delayedSegments) = runB bpm $ fromMono . toMono <$> compileSong song

-- Previews a song by removing all delays.
previewSong :: Song -> IO ()
previewSong (Song bpm delayedSegments) = runB bpm . compileSong $ Song bpm (removeDelays delayedSegments)
