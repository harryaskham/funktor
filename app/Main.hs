module Main where

import Csound.Base
import Csound.Patch
import Scratch
import Tools
import FirstSong
import Note

main :: IO ()
main = runToDisk minSong
