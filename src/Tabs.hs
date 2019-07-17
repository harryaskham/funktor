module Tabs (DrumTab(..), compileTabs) where

import Csound.Base
import Csound.Patch
import Csound.Sam
import Csound.Sam.Core
import Data.List.Split

-- A pairing of intensity tab and sample to play.
-- Visual drum sequencer.
-- _ = empty beat
-- . = light beat
-- o = mid beat
-- O = heavy beat
-- | = bar separator
-- ' ' = beat separator
data DrumTab = DrumTab String Sam 

type BeatLength = D
type BeatVelocity = D
type Beat = (BeatLength, BeatVelocity)

-- Takes a list of drum tracks and compiles to a signal.
compileSample :: Sig -> Sample Sig2 -> SE Sig2
compileSample bpm = runSam (bpm * 4)

-- Takes those tabs and turns em into musak
compileTabs :: Sig -> [DrumTab] -> SE Sig2
compileTabs bpm = compileSample bpm . sum . fmap compileTab

-- | Compiles a tab intno a polyphonic signal.
compileTab :: DrumTab -> Sample Sig2
compileTab (DrumTab t s) = pat' velocities (sig <$> lengths) s
  where
    beats = concat $ compileBar <$> splitOn "|" t
    (lengths, velocities) = unzip beats

-- | Compiles the given bar segment into its constituent beats.
compileBar :: String -> [Beat]
compileBar bar = compileBeat <$> splitBar bar

-- | Compiles a single beat string
compileBeat :: String -> Beat
compileBeat b@('_':_) = (fromIntegral $ length b, 0.0)
compileBeat b@('.':_) = (fromIntegral $ length b, 0.4)
compileBeat b@('o':_) = (fromIntegral $ length b, 0.7)
compileBeat b@('O':_) = (fromIntegral $ length b, 0.9)
compileBeat b@('X':_) = (fromIntegral $ length b, 1.0)
compileBeat b = error $ "Invalid beat: " ++ b

-- Split the given bar into its constituent beat strings
splitBar :: String -> [String]
splitBar = filter (not . null) . splitOn " "
