module Tools where

import Data.List
import Csound.Base
import Csound.Sam

type Spb = Sig

-- Run the given song respecting the global Bpm.
runB :: Bpm -> SE Sig2 -> IO ()
runB bpm song = dac $ setBpm bpm >> song

-- Writes song to disk in offline-render (useful for working on Mac with earphones, which csound hates)
runToDisk :: SE Sig2 -> IO ()
runToDisk = writeSnd "tmp.wav"

-- Helper to transform the CsdNote.
cpspch2 :: SigOrD a => (D, a) -> (D, a)
cpspch2 (v, n) = (v, cpspch n)

-- Given increasingly intense things, produces a set of incremental things with each new one added.
increasingSequences :: [a] -> [[a]]
increasingSequences = tail . inits

-- Get the spb from the bpm
spb :: Bpm -> Spb
spb bpm = 60 / bpm 

-- Allows us to loop a signal, not just a segment
loopSig :: Sig2 -> Sig2
loopSig = runSeg . loop . toSeg

-- ALlows us to limit a signal, not just a segment
limSig :: Sig -> Sig2 -> Sig2
limSig bars = runSeg . constLim (takt bars) . toSeg
