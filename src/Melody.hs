module Melody where

import Csound.Base
import Csound.Patch
import Csound.Sam
import Tools
import Note

data MelSegment = MelSegment Bpm Patch2 (Track Sig (D, D))

-- Compiles the given track using the given patch.
compileMelody :: MelSegment -> Sig2
compileMelody (MelSegment bpm patch track) = mix . atSco patch . fmap cpspch2 . str (spb bpm) $ track

-- Introduces silence at the start of the segment
withDelay :: Sig -> MelSegment -> MelSegment
withDelay delay (MelSegment bpm patch track) = MelSegment bpm patch $ mel [toMel [Silent delay], track]
