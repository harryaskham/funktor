module Main where

import Csound.Base
import Csound.Patch
import Csound.Sam
import Csound.Sam.Core
import Tabs
import Note
import Melody
import HouseSong
import Tetris
import Tools

main :: IO ()
main = runToDisk houseSong
