module Melody where

import Csound.Base
import Csound.Patch
import Csound.Sam
import Tools

-- Compiles the given track using the given patch.
compileMelody :: Bpm -> Patch2 -> Track Sig (D, D) -> Sig2
compileMelody bpm patch = mix . atSco patch . fmap cpspch2 . str (spb bpm)
