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

-- A song represented as the parallel segments to play for the given duration
data DelayedSegment = DelayedSegment TrackSegment Sig
                    | DelayedDrums Drums Sig

-- A combination of delayed segments and drum information.
newtype Song = Song [DelayedSegment]

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
compileDelayedSegment :: DelayedSegment -> SE Sig2
compileDelayedSegment (DelayedSegment t d) = pure . compileSegment $ withDelay d t
compileDelayedSegment (DelayedDrums drums d) = delaySnd (syn d) <$> drums

-- Compile the given delayed segments into their corresponding signal.
compileDelayedSegments :: [DelayedSegment] -> SE Sig2
compileDelayedSegments = sum . fmap compileDelayedSegment

-- Removes the delays so that segments can be previewed all at once.
removeDelays :: [DelayedSegment] -> [DelayedSegment]
removeDelays = fmap withNoDelay
  where
    withNoDelay (DelayedSegment t _) = DelayedSegment t 0
    withNoDelay (DelayedDrums d _) = DelayedDrums d 0

-- Compiles a song down to its signal
compileSong :: Song -> SE Sig2
compileSong (Song delayedSegments) = compileDelayedSegments delayedSegments

-- Previews a song by removing all delays.
previewSong :: Song -> SE Sig2
previewSong (Song delayedSegments) = compileSong $ Song (removeDelays delayedSegments)
