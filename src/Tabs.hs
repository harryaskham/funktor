{-# LANGUAGE FlexibleContexts #-}

module Tabs where

import Control.Lens
import Control.Monad.Reader
import Csound.Base
import Csound.Patch
import Csound.Sam
import Csound.Sam.Core
import Data.Bifunctor
import qualified Data.List.Safe as LS
import Data.List.Split
import Data.Maybe
import Debug.Trace
import System.Random
import Tools

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

type Beat = (BeatLength, Maybe BeatVelocity)

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
compileWithDropOut :: Double -> Bpm -> DrumTab -> IO (SE Sig2)
compileWithDropOut dot bpm tab = do
  g <- newStdGen
  return $ compileTabsDropOut bpm (dropOut g dot) (pure tab)

-- Merges any empty beats into the beat before.
-- For beats at the start, a rest sample is introduced
mergeEmptyBeats :: [Beat] -> [Beat]
mergeEmptyBeats [] = []
mergeEmptyBeats [b] = [b]
mergeEmptyBeats ((l1, Just v1) : (l2, Just v2) : bs) = (l1, Just v2) : (l2, Just v2) : mergeEmptyBeats bs
mergeEmptyBeats ((l1, Nothing) : (l2, Nothing) : bs) = mergeEmptyBeats ((l1 + l2, Nothing) : bs)
mergeEmptyBeats ((l1, Just v1) : (l2, Nothing) : bs) = mergeEmptyBeats ((l1 + l2, Just v1) : bs)
mergeEmptyBeats ((l1, Nothing) : (l2, Just v2) : bs) = (l1, Nothing) : mergeEmptyBeats ((l2, Just v2) : bs)

-- | Compiles a tab intno a polyphonic signal.
-- | Can be used to introduce dropout.
compileTab :: [DropOut] -> DrumTab -> Sam
compileTab dropOut dt@(DrumTab t s) = startRest =:= pat' (fromMaybe 0.0 <$> velocities') (sig <$> lengths') s
  where
    -- First parse the tab string
    beats = compileTabToBeats dt
    -- Apply any dropout. Could have used rndPat here
    dropOutBeats = applyDropout dropOut beats
    -- Here, if a beat is missing, we want to merge its length with the previous beat
    -- This avoids tonnes of empty beats
    mergedBeats = mergeEmptyBeats dropOutBeats
    -- Finally we can't merge the first beat with anything so we introduce a rest instead.
    -- No rest if the first beat has content. If we intro the rest, we need to drop the first
    -- empty beat
    (lengths, velocities) = unzip mergedBeats
    (startRest, lengths', velocities') =
      case snd <$> LS.head mergedBeats of
        Nothing -> (rest . sig . fst $ head mergedBeats, tail lengths, tail velocities)
        Just _ -> (rest 0, lengths, velocities)

-- | Takes the list of dropouts and uses them to set beat velocities to zero.
applyDropout :: [DropOut] -> [Beat] -> [Beat]
applyDropout ds bs = uncurry newBeat <$> zip ds bs
  where
    newBeat :: DropOut -> Beat -> Beat
    newBeat d b = case d of
      DropOut -> second (const Nothing) b
      DropIn -> b

-- | Compiles a list of tabs into a beat.
compileTabs :: Bpm -> [DrumTab] -> SE Sig2
compileTabs bpm = compileSamples bpm . fmap (compileTab $ repeat DropIn)

-- | Compiles a list of tabs into a beat.
compileTabsDropOut :: Bpm -> [DropOut] -> [DrumTab] -> SE Sig2
compileTabsDropOut bpm dropOut = compileSamples bpm . fmap (compileTab dropOut)

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
    normalizeBeat = first (* modifier)

-- | Compiles the given bar segment into its constituent beats.
compileBar :: String -> [Beat]
compileBar = normalizeBar . fmap compileBeat . splitBar

-- | Get the length of the bar,
barLength :: [Beat] -> D
barLength = fromIntegral . length

-- | Compiles a single beat string
compileBeat :: String -> Beat
compileBeat b@('_' : _) = (fromIntegral $ length b, Nothing)
compileBeat b@('.' : _) = (fromIntegral $ length b, Just 0.2)
compileBeat b@('o' : _) = (fromIntegral $ length b, Just 0.4)
compileBeat b@('O' : _) = (fromIntegral $ length b, Just 0.6)
compileBeat b@('X' : _) = (fromIntegral $ length b, Just 0.8)
compileBeat b = error $ "Invalid beat: " ++ b

-- Split the given bar into its constituent beat strings
splitBar :: String -> [String]
splitBar = filter (not . null) . splitOn " "

-- Tool for monadically compiling drums using BPM from environment
-- Also hides SE in the monad stack for nicer mixing with regular signals.
compileD :: (MonadReader SongEnv m, MonadSE m) => DrumTab -> m (Seg Sig2)
compileD tab = do
  bpm <- asks (view bpm)
  compiled <- liftSE $ compileTabs bpm [tab]
  return $ toSeg compiled

-- Convenience  wrapper  around monadic  drum creation
drums :: (MonadReader SongEnv m, MonadSE m) => String -> Sam -> m (Seg Sig2)
drums tab sam = compileD (DrumTab tab sam)

-- Monadic dropout drums.
drumsDr :: (MonadReader SongEnv m, MonadSE m, MonadIO m) => String -> Sam -> Double -> m (Seg Sig2)
drumsDr tab sam dropout = do
  bpm <- asks (view bpm)
  compiled <- liftIO $ compileWithDropOut dropout bpm (DrumTab tab sam)
  toSeg <$> liftSE compiled

-- Convenience function
drumsDrop :: (MonadReader SongEnv m, MonadSE m, MonadIO m) => [Char] -> Sam -> Double -> Int -> m (Seg Sig2)
drumsDrop tab sam amount times = drumsDr (concat $ replicate times tab) sam amount
