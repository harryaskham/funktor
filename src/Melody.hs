module Melody where

import Csound.Base
import Csound.Patch
import Csound.Sam
import Tools
import Note

-- Definition of a song segment including BPM and instrument information.
data Segment a = Segment Bpm Patch2 a

-- Allow us to map over segments to create morphing track pieces.
instance Functor Segment where
  fmap f (Segment bpm patch a) = Segment bpm patch (f a)

-- The type of segment we're using here.
type TrackSegment = Segment (Track Sig (D, D))

-- A song represented as the parallel segments to play for the given duration
data DelayedSegment = DelayedSegment TrackSegment Sig

-- Compiles the given delayed segment to a track segment with its delay
compileDelayedSegment :: DelayedSegment -> TrackSegment
compileDelayedSegment (DelayedSegment t d) = withDelay d t

-- Removes the delays so that segments can be previewed all at once.
removeDelays :: [DelayedSegment] -> [DelayedSegment]
removeDelays = fmap withNoDelay
  where
    withNoDelay (DelayedSegment t _) = DelayedSegment t 0

-- Compile the given segment as a Seg
compileToSeg :: TrackSegment -> Seg Sig2
compileToSeg = toSeg . compileSegment

-- Compiles the given segment to a signal
compileSegment :: TrackSegment -> Sig2
compileSegment (Segment bpm patch track) = mix . atSco patch . fmap cpspch2 . str (spb bpm) $ track

-- Introduces silence at the start of the segment
withDelay :: Sig -> TrackSegment -> TrackSegment
withDelay delay = fmap (mel . (toMel [Silent delay]:) . pure)
