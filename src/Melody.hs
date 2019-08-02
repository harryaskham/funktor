module Melody where

import Csound.Base
import Csound.Patch
import Csound.Sam
import Tools
import Note

-- Definition of a song segment including BPM and instrument information.
data Segment a = Segment Bpm Patch2 a
type TrackSegment = Segment (Track Sig (D, D))

-- Allow us to map over segments to create morphing track pieces.
instance Functor Segment where
  fmap f (Segment bpm patch a) = Segment bpm patch (f a)

-- Compile the given segment as a Seg
compileToSeg :: TrackSegment -> Seg Sig2
compileToSeg = toSeg . compileSegment

-- Compiles the given segment to a signal
compileSegment :: TrackSegment -> Sig2
compileSegment (Segment bpm patch track) = mix . atSco patch . fmap cpspch2 . str (spb bpm) $ track

-- Introduces silence at the start of the segment
withDelay :: Sig -> TrackSegment -> TrackSegment
withDelay delay = fmap (mel . (toMel [Silent delay]:) . pure)
