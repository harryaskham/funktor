module Main where

import Csound.Base

bpm = 140

bps = bpm / 60

sin4 = osc bps

sin8 = osc $ 2 * bps

sin1 = osc $ bps / 4

drums = sin8 * tri 110

main :: IO ()
main = dac $ drums
