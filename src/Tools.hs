{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Tools where

import Data.List
import Csound.Base hiding (random)
import Csound.Sam
import System.Random
import Control.Monad
import Control.Lens hiding (at)
import Control.Monad.Reader
import Control.Monad.Trans.IO
import Control.Monad.Random

-- TODO: rethink the melody env
-- TODO: beatLength in the Env is awful.
--       only needed to replicate melodies forever
--       so they dont run out but this is a bug.
data SongEnv = SongEnv { _bpm :: Bpm
                       , _beatLength :: Int
                       }
makeLenses ''SongEnv

-- MTL stack for song creation.
-- Has an environment in SongEnv
-- Has SE to allow drums alongside computational waveforms
-- Also has IOT to enable e.g. randomness
type SongT = ReaderT SongEnv (RandT StdGen (IOT SE))
type SongM = SongT (Seg Sig2)

-- A MonadIO like class for SE
class (Monad m) => MonadSE m where
  liftSE :: SE a -> m a

instance MonadSE SE where
  liftSE = id

instance MonadSE (IOT SE) where
  liftSE = lift . liftSE

instance (RandomGen g, MonadSE m) => MonadSE (RandT g m) where
  liftSE = lift . liftSE

instance MonadSE SongT where
  liftSE = lift . liftSE

-- Kind of a valid runner, but also handles seg -> sig conversion
runSongM :: SongEnv -> SongM -> IO (SE Sig2)
runSongM env song = do
  g <- newStdGen
  runSeg <$$> runIOT (evalRandT (runReaderT song env) g)

instance Semigroup SongM where
  a <> b = cotraverse mel [a, b]

instance Monoid SongM where
  mempty = return $ constRest 0

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
-- No longer needed - defined by lens
-- infixl 4 ??
-- (??) :: (Functor f) => f (a -> b) -> a -> f b
-- f ?? a = ($ a) <$> f

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
beatsToHz = (1/) . beatsToSecs

-- Allows us to loop a signal, not just a segment
loopSig :: Sig2 -> Sig2
loopSig = runSeg . loop . toSeg

concatSig :: (Sigs a) => a -> a -> a
concatSig s1 s2 = runSeg $ mel [toSeg s1, toSeg s2]

concatSigs :: (Sigs a) => [a] -> a
concatSigs = runSeg . loop . mel . fmap toSeg

-- A number of beats at a given BPM.
data Beats = Beats Bpm Sig

-- ALlows us to limit a signal, not just a segment
limSig :: Beats -> Seg a -> Seg a
limSig beats = constLim (beatsToSecs beats)

delSig :: Num a => Beats -> Seg a -> Seg a
delSig beats = constDel (beatsToSecs beats)

restSig :: Beats -> Seg Sig2
restSig beats = constRest (beatsToSecs beats)

-- Play the given segment for only the number of beats given.
forBeats :: (MonadReader SongEnv m, Sigs a) => Sig -> Seg a -> m (Seg a)
forBeats n seg = do
  bpm <- asks (view bpm)
  return $ limSig (Beats bpm n) seg

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
doN :: Int -> (a -> a) -> a -> a
doN 0 _ = id
doN n f = foldl1 (.) (replicate n f)

-- Do N times monadically
doNM :: (Monad m) => Int -> (a -> m a) -> a -> m a
doNM 0 _ = return
doNM n f = foldl1 (>=>) (replicate n f)

pink2 = fromMono <$> pink
brown2 = fromMono <$> brown

stereoMap :: (a -> b) -> (a, a) -> (b, b)
stereoMap f (a1, a2) = (f a1, f a2)

-- Reduces across set of monadic values.
-- Ah - turns out this is "cotraverse" in the Distributive category...
cotraverse :: (Monad m, Traversable t) => (t a -> a) -> t (m a) -> m a
cotraverse f as = f <$> sequence as

-- Convenience combination of cotraverse har over SE segments.
cotHar :: (MonadSE m) => [SE (Seg Sig2)] -> m (Seg Sig2)
cotHar = liftSE . cotraverse har

randomFrom :: (MonadIO m) => [a] -> m a
randomFrom xs = (xs !!) <$> liftIO (randomRIO (0, length xs - 1))

randEnum :: (Enum a, Bounded a, MonadIO m) => m a
randEnum = randomFrom [minBound..maxBound]

takeIxs :: [Int] -> [a] -> [a]
takeIxs ixs xs = pure (xs !!) <*> ixs

-- Okay weirdly, once we run once with the Mac options,
-- we then apparently get permanently enabled on mac.
-- The options to dacBy when using jabras with the mac
macJabraOpts :: Options
macJabraOpts =
  def
  <> setCoreAudio
  <> setRates 44100 32
  <> setBufs 512 1024
  <> setOutput "dac:1"
  <> setInput "adc:2"
