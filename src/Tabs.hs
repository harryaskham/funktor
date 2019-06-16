module Tabs (DrumTab(..), compileTab) where

import Csound.Base
import Csound.Patch
import Csound.Sam
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
type Velocity = D
type Beat = (BeatLength, Velocity)

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
compileBeat b@('.':_) = (fromIntegral $ length b, 0.6)
compileBeat b@('o':_) = (fromIntegral $ length b, 0.9)
compileBeat b@('O':_) = (fromIntegral $ length b, 1.0)
compileBeat b = error $ "Invalid beat: " ++ b

-- Split the given bar into its constituent beat strings
-- TODO: Remove reliance on space-splitting
splitBar :: String -> [String]
splitBar = filter (not . null) . splitOn " "
