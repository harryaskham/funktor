module Main where

import Csound.Base
import qualified Scratch

-- Create simple drum sequences
-- Use >>= to create layered combinations
-- Other functional stuff to chop-and-screw
-- Create other seqs of e.g. bass, lead, etc
-- >>= them all together
-- Use some metric over the space to move through it bar-by-bar in interesting way

main :: IO ()
main = dac Scratch.song
