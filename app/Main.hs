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
import Tools
import PlaneToSJC
import LofiAttempt
import AirportTechno
import ArpAttempt
import LatePlane
import Saturday
import HouseMonad
import DnbPlayground
import TechnoPlayground
import Garage
import TableTest
import ArpPatterns
import Series
import Sample
import Sticks
import SticksTwo
import SticksThree
import ExampleTrack
import Overground

main :: IO ()
main = runToDisk =<< gar'
