module Main where

import Csound.Base
import Csound.Patch
import Csound.Sam
import Csound.Sam.Core
import Tabs
import Croatia
import Note
import Melody
import HouseSong
import Tetris
import Tools
import PlaneToSJC
import LofiAttempt

main :: IO ()
main = do
  s <- LofiAttempt.song
  runToDisk s
