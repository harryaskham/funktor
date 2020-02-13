{-# LANGUAGE FlexibleContexts #-}

module Tabs where

import Csound.Base
import Csound.Patch
import Csound.Sam
import Csound.Sam.Core
import Data.List.Split
import Data.Bifunctor
import Tools
import System.Random
import Control.Monad.Reader

-- Number of beats after which to truncate the tab.
type TabLength = Int

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

-- Debug runner to test out a tab at 140 bpm.
runTab :: DrumTab -> IO ()
runTab = runB 140 . compileTabs 140 . pure

-- Takes a list of drum tracks and compiles to a signal.
compileSample :: Bpm -> Sam -> SE Sig2
compileSample bpm = runSam (bpm * 4)

-- Compiles only the given samples, at the given bpm
compileSamples :: Bpm -> [Sam] -> SE Sig2
compileSamples bpm = compileSample bpm . sum

-- Compiles a tab to its list of beats, and replicate until we took enough beats.
compileTabToBeats :: DrumTab -> [Beat]
compileTabToBeats (DrumTab t s) = concat $ compileBar <$> splitOn "|" t

-- Compiles a drum tab with dropout.
-- TODO: Merge IO/SE
compileWithDropOut :: Double -> Bpm -> DrumTab -> IO (SE Sig2)
compileWithDropOut dot bpm tab = do
  g <- newStdGen
  return $ compileTabsDropOut bpm (dropOut g dot) (pure tab)

-- | Compiles a tab intno a polyphonic signal.
-- | Can be used to introduce dropout.
compileTab :: [DropOut] -> DrumTab -> Sam
compileTab dropOut dt@(DrumTab t s) = pat' velocities (sig <$> lengths) s
  where
    beats = compileTabToBeats dt
    dropOutBeats = applyDropout dropOut beats
    (lengths, velocities) = unzip dropOutBeats

-- | Takes the list of dropouts and uses them to set beat velocities to zero.
applyDropout :: [DropOut] -> [Beat] -> [Beat]
applyDropout ds bs = uncurry newBeat <$> zip ds bs
  where
    newBeat :: DropOut -> Beat -> Beat
    newBeat d b = case d of
                    DropOut -> second (const 0.0) b
                    DropIn -> b

-- | Compiles a list of tabs into a beat.
compileTabs :: Bpm -> [DrumTab] -> SE Sig2
compileTabs bpm = compileSamples bpm . fmap (compileTab $ repeat DropIn)

-- | Compiles a list of tabs into a beat.
compileTabsDropOut :: Bpm -> [DropOut] -> [DrumTab] -> SE Sig2
compileTabsDropOut bpm dropOut  = compileSamples bpm . fmap (compileTab dropOut)

-- | Compiles a sequence of tabs one after the other, with the given limit.
-- | Ends up looping the final beat forever.
compileTabSequenceWithLoop :: Bpm -> Sig -> [[DrumTab]] -> SE Sig2
compileTabSequenceWithLoop = compileTabSequenceWithLimiter mapToAllButLast

-- | Compiles a sequence of tabs one after the other, with the given limit.
-- | The final beat is also truncated.
compileTabSequence :: Bpm -> Sig -> [[DrumTab]] -> SE Sig2
compileTabSequence = compileTabSequenceWithLimiter map

compileTabSequenceWithLimiter :: ((Sam -> Sam) -> [Sam] -> [Sam]) -> Bpm -> Sig -> [[DrumTab]] -> SE Sig2
compileTabSequenceWithLimiter mapper bpm limit tabLists = compileSample bpm limitedSams
  where
    compiledSams = sum <$> compileTab (repeat DropIn) <$$> tabLists
    limitedSams = flow $ mapper (lim limit) compiledSams

-- | Ensures the given bar lengths are appropriate for the number of beats in the bar.
-- | Uses 4/4 as a baseline before manipulating lengths.
-- | e.g. o o o o would be a modifier of 1.0
-- | and o o o o o o o o would be a modifier of 0.5
normalizeBar :: [Beat] -> [Beat]
normalizeBar bar = normalizeBeat <$> bar
  where
    modifier = 4.0 / barLength bar
    normalizeBeat = first (*modifier)

-- | Compiles the given bar segment into its constituent beats.
compileBar :: String -> [Beat]
compileBar = normalizeBar . fmap compileBeat . splitBar

-- | Get the length of the bar,
barLength :: [Beat] -> D
barLength = fromIntegral . length

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

-- Tool for monadically compiling drums using BPM from environment
-- Also hides SE in the monad stack for nicer mixing with regular signals.
compileD :: (MonadReader Bpm m, MonadSE m) => DrumTab -> m (Seg Sig2)
compileD tab = do
  bpm <- ask
  compiled <- liftSE $ compileTabs bpm [tab]
  return $ toSeg compiled

-- Convenience  wrapper  around monadic  drum creation
drums :: (MonadReader Bpm m, MonadSE m) => String -> Sam -> m (Seg Sig2)
drums tab sam = compileD (DrumTab tab sam)
