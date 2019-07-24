module Tools where

import Csound.Base

type BPM = Sig

-- Run the given song respecting the global BPM.
run :: SE Sig2 -> BPM -> IO ()
run song bpm = dac $ setBpm bpm >> song

-- Writes song to disk in offline-render (useful for working on Mac with earphones, which csound hates)
runToDisk :: SE Sig2 -> IO ()
runToDisk = writeSnd "tmp.wav"

-- Helper to transform the CsdNote.
cpspch2 :: SigOrD a => (D, a) -> (D, a)
cpspch2 (v, n) = (v, cpspch n)

-- Given increasingly intense things, produces a set of incremental things with each new one added.
increasingSequences :: [a] -> [[a]]
increasingSequences ts = take <$> [1..length ts] <*> pure ts

-- Get the spb from the bpm
spb :: BPM -> Sig
spb bpm = 60 / bpm 
