{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}

module Melody where

import Csound.Base hiding (Duration)
import Csound.Patch
import Csound.Sam
import Tools
import Note
import Data.Tuple.Extra
import Control.Lens
import Control.Monad.Reader
import Debug.Trace

-- A convenience alias for compiled drums.
type Drums = SE Sig2

-- Definition of a song segment including BPM and instrument information.
data Segment a = Segment Bpm Patch2 a deriving (Functor)

-- The type of segment we're using here.
type TrackSegment = Segment (Track Sig (D, D))

-- The delay of a segment in beats.
newtype SegDelay = SegDelay Sig

-- The duration of a segment in beats.
newtype SegDuration = SegDuration Sig

-- An envelope for a segment.
type SegEnv = Sig

-- Add envelope to signal neatly
withEnv :: SegEnv -> Seg Sig2 -> Seg Sig2
withEnv e = fmap $ stereoMap (e*)

-- TODO: Delayable could be used here to reconcile the delayed segment
-- piece with class constraint on first data element?

-- A song represented as the parallel segments to play for the given duration
data DelayedSegment = DelayedSegment TrackSegment SegDelay SegDuration
                    | DelayedDrums Drums SegDelay SegDuration
                    | EnvSegment TrackSegment SegEnv
                    | EnvDrums Drums SegEnv

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

-- Compiles the given trac
compileTrack :: Bpm -> Patch2 -> Track Sig (D, D) -> Sig2
compileTrack bpm patch = mix . atSco patch . fmap cpspch2 . str (spb bpm)

-- Compiles the given segment to a signal
compileSegment :: TrackSegment -> Sig2
compileSegment (Segment bpm patch track) = compileTrack bpm patch track

-- Helper to run a segment by itself in isolation.
runSegment :: TrackSegment -> IO ()
runSegment seg@(Segment bpm patch notes) = runB bpm $ pure $ compileSegment seg

-- Compiles the given delayed segment to a track segment with its delay
-- TODO: Migrate to only using envelopes.
compileDelayedSegment :: Bpm -> DelayedSegment -> SE Sig2
compileDelayedSegment bpm (DelayedSegment t (SegDelay del) (SegDuration dur)) = pure delayed
  where
    compiled = compileSegment t
    limited = runSeg $ limSig (Beats bpm dur) $ toSeg compiled
    delayed = delaySnd (beatsToSecs (Beats bpm del)) limited
compileDelayedSegment bpm (DelayedDrums drums (SegDelay del) (SegDuration dur)) = delayed
  where
    limited = runSeg . limSig (Beats bpm dur) . toSeg <$> drums
    delayed = delaySnd (beatsToSecs (Beats bpm del)) <$> limited
compileDelayedSegment bpm (EnvSegment t env) = pure $ fromMono env * compileSegment t
compileDelayedSegment bpm (EnvDrums drums env) = (fromMono env *) <$> drums

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

-- A square envelope that will be on and off for the given number of bars.
sqrEnv :: Bpm -> D -> Sig -> SegEnv
sqrEnv bpm phase onFor = usqr' phase (beatsToHz $ Beats bpm (onFor * 2))

sqrEnvM :: (MonadReader SongEnv m) => D -> Sig -> m SegEnv
sqrEnvM = liftFst3 sqrEnv (asks $ view bpm)

-- A sin env that will flow in and out per the phase needed.
sinEnv :: Bpm -> D -> Sig -> SegEnv
sinEnv bpm phase onFor = uosc' phase (beatsToHz $ Beats bpm (onFor * 2))

-- Monadic version of sinEnv
sinEnvM :: (MonadReader SongEnv m) => D -> Sig -> m SegEnv
sinEnvM = liftFst3 sinEnv (asks $ view bpm)

-- A ramp env
rampEnv :: Bpm -> Sig -> D -> Sig -> SegEnv
rampEnv bpm ram phase onFor = uramp' ram phase (beatsToHz $ Beats bpm (onFor * 2))

-- Inserts the necessary 0-length points to allow us to compile the table
expandTable [] = []
expandTable (amp:dur:xs) = amp:dur:amp:0:expandTable xs

data SqrTabOnOff = OnFor Double | OffFor Double

-- Tabulated env with square convenience function
sqrTabEnv :: (MonadReader SongEnv m) => [SqrTabOnOff] -> m Sig
sqrTabEnv pattern = do
  gBPM <- asks (view bpm)
  return
    $ ublosc
        (lins $ expandTable convPattern)
        (beatsToHz $ Beats gBPM (sig . double $ patLength))
  where
    -- Convert the onoff to our partial square
    convOnOff (OnFor d) = [1, d]
    convOnOff (OffFor d) = [-1, d]
    convPattern = concat $ convOnOff <$> pattern
    -- Infer the pattern length from the lengths of the segments
    -- Only works as long as we have at least two segments
    patLength = sum $ (convPattern !!) <$> [1,3..length convPattern - 1]

-- A constantly-on envelope.
constEnv :: SegEnv
constEnv = 1

-- A constantly-off envelope,
offEnv :: SegEnv
offEnv = 0

-- Take some instrument gens and create the corresponding verse.
-- Useful for IO-bound track segments.
genEnvSegs :: Functor f => f (Note -> IO TrackSegment) -> Note -> SegEnv -> f (IO DelayedSegment)
genEnvSegs instrs root env = EnvSegment <$$> (instrs ?? root) ??? env

-- Adds a drop to the given segment.
-- TODO: Avoid the explicit length passing
withDrop :: (MonadReader SongEnv m) => Sig -> Sig -> Seg Sig2 -> Seg Sig2 -> m (Seg Sig2)
withDrop len delay drop seg = do
  bpm <- asks (view bpm)
  let newSeg = limSig (Beats bpm delay) seg +:+ restSig (Beats bpm len)
      newDrop = restSig (Beats bpm delay) +:+ limSig (Beats bpm len) drop
  return $ newSeg =:= newDrop

-- Tool for monadically compiling instrument using BPM from environment
-- Ensures it plays for duration of song
-- TODO: Revisit this once we can do instrument segments better without leakage.
compileI :: (MonadReader SongEnv m) => Patch2 -> [Pch] -> m (Seg Sig2)
compileI instr notes = do
  bpm <- asks (view bpm)
  beatLength <- asks (view beatLength)
  return $ toSeg $ compileTrack bpm instr (toMel (repeatToBeats beatLength notes))
