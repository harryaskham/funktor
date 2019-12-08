module Tools where

import Data.List
import Csound.Base hiding (random)
import Csound.Sam
import System.Random
import Control.Monad

type Spb = Sig

-- Double nested fmap
infixl 5 <$$>
(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap

-- Double nested ap
infixl 5 <***>
(<***>) :: (Applicative f, Applicative g) => f (g (a -> b)) -> g a -> f (g b)
f <***> a = (<*> a) <$> f

-- Flap function
infixl 4 ??
(??) :: (Functor f) => f (a -> b) -> a -> f b
f ?? a = ($ a) <$> f

-- Double nested Flap function
infixl 4 ???
(???) :: (Functor f, Functor g) => f (g (a -> b)) -> a -> f (g b)
f ??? a = ($ a) <$$> f

-- fmap with ZipList coercion
(<$+>) :: (a -> b) -> [a] -> ZipList b
f <$+> a = f <$> ZipList a

-- ap with ZipList coercion
(<*+>) :: ZipList (a -> b) -> [a] -> ZipList b
f <*+> a = f <*> ZipList a

-- Final-chain ap with ZipList coercion allowing f <$+> a <$++> b
(<*++>) :: ZipList (a -> b) -> [a] -> [b]
f <*++> a = getZipList $ f <*+> a

-- Lift only the first arg into the functor.
liftFst3 :: Functor f => (a -> b -> c -> d) -> f a -> b -> c -> f d
liftFst3 f a b c = f <$> a ?? b ?? c

-- Run the given song respecting the global Bpm.
runB :: Bpm -> SE Sig2 -> IO ()
runB bpm song = dac $ setBpm bpm >> song

-- Writes song to disk in offline-render (useful for working on Mac with earphones, which csound hates)
runToDisk :: SE Sig2 -> IO ()
runToDisk = writeSnd "tmp.wav"

-- Helper to transform the CsdNote.
cpspch2 :: SigOrD a => (D, a) -> (D, a)
cpspch2 (v, n) = (v, cpspch n)

-- Given increasingly intense things, produces a set of incremental things with each new one added.
increasingSequences :: [a] -> [[a]]
increasingSequences = tail . inits

-- Get the spb from the bpm
spb :: Bpm -> Spb
spb bpm = 60.0 / bpm

-- Gets the number of seconds corresponding to the given number of beats.
beatsToSecs :: Beats -> Sig
beatsToSecs (Beats bpm beats) = spb bpm * beats

beatsToHz :: Beats -> Sig
beatsToHz = (1/) <$> beatsToSecs

-- Allows us to loop a signal, not just a segment
loopSig :: Sig2 -> Sig2
loopSig = runSeg . loop . toSeg

-- A number of beats at a given BPM.
data Beats = Beats Bpm Sig

-- ALlows us to limit a signal, not just a segment
limSig :: Beats -> Sig2 -> Sig2
limSig beats = runSeg . constLim (beatsToSecs beats) . toSeg

-- Maps the given function to all but the last member of a list.
mapToAllButLast :: (a -> a) -> [a] -> [a]
mapToAllButLast f xs = reverse $ head rev : fmap f (tail rev)
 where
   rev = reverse xs

-- Phases in and out over two bars
-- TODO: Almost certainly needs regenerating
inOutFilter :: SigSpace a => a -> a
inOutFilter = at (mlp (1000 + 4000 * uosc (takt 4)) 0.55)

-- Loop the given list of things n times
dup :: Integer -> [a] -> [a]
dup = (>>) . enumFromTo 1

-- Convenience functions for encoding delays and durations
beats :: Sig -> Sig
beats = id

bars :: (Num a) => a -> a
bars = (*4)

toSig :: Int -> Sig
toSig = sig . int

-- A dropout type.
data DropOut = DropIn | DropOut deriving (Bounded, Enum, Show)

-- Dropout with a given probability.
dropOut :: RandomGen g => g -> Double -> [DropOut]
dropOut g p = (if x < p then DropOut else DropIn) : dropOut g' p
  where
    (x, g') = random g

-- A cyclical version of succ
succC :: (Eq a, Enum a, Bounded a) => a -> a
succC a = if a == maxBound then minBound else succ a

-- A cyclical version of pred
predC :: (Eq a, Enum a, Bounded a) => a -> a
predC a = if a == minBound then maxBound else pred a

-- Do N times
doN n f = foldr (.) id (replicate n f)

pink2 = fromMono <$> pink
brown2 = fromMono <$> brown
