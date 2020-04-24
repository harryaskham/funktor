module Series where

import Csound.Base hiding (Tab, clp, Duration)
import Csound.Sam
import Csound.Patch
import Tabs
import Tools
import Note
import Melody
import Data.Sort
import Data.Ord
import Control.Lens
import Csound.Catalog.Drum.Tr808 as Tr808
import Csound.Catalog.Drum.Hm as Hm
import Csound.Catalog.Effect
import System.IO.Unsafe
import Control.Monad.Reader
import Control.Monad.Random
import Data.List
import System.Random
import System.Random.Shuffle

primes :: [Integer]
primes = 2 : primes'
  where isPrime (p:ps) n = p*p > n || n `rem` p /= 0 && isPrime ps n
        primes' = 3 : filter (isPrime primes') [5, 7 ..]

fibSeries :: [Integer]
fibSeries = 1:1:fibFrom 1 1
  where
    fibFrom a b = c:fibFrom b c
      where
        c = a + b

cyclicGet :: [a] -> Int -> a
cyclicGet xs i = xs !! (i `mod` length xs)
