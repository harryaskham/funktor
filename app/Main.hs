module Main where

import Csound.Base
import Csound.Patch
import Scratch
import Tools
import FirstSong

main :: IO ()
main = dac scratchSong
