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
import AirportTechno
import ArpAttempt
import AuldLangSyne
import LatePlane
import Saturday

main :: IO ()
main = runToDisk =<< LofiAttempt.song
