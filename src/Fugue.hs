{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Fugue where

import Csound.Base hiding (Tab, clp, Duration)
import Csound.Sam
import Csound.Patch
import Tabs
import Tools
import Note
import Melody
import Sample
import Series
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

data Fugue = Fugue { _theme :: [Note]
                   , _octaves :: [Octave]
                   , _voices :: [[Note] -> [Octave] -> [Pch]]
                   , _voiceEnvs :: [SegEnv]
                   , _instruments :: [Patch2]
                   , _enterBeats :: Int
                   }
makeLenses ''Fugue

compileFugue :: (MonadReader SongEnv m, MonadRandom m) => Fugue -> m (Seg Sig2)
compileFugue f = do
  bs <- asks (view beatLength)
  introEnvs <- traverse (\i -> sqrTabEnv [OffFor (fromIntegral $ i*f^.enterBeats), OnFor $ fromIntegral bs]) [0..20]
  let themeNotes = (f^.voices) ?? (f^.theme) ?? (f^.octaves)
  compiledVoices <-
    sequence
    $ getZipList
    $ ZipList (compileI <$> f^.instruments)
    <*> ZipList themeNotes
  return
    $ har
    $ compiledVoices
    & zipEnvsWith (\e1 e2 -> minabs [e1,e2]) introEnvs
    & zipEnvsWith (\e1 e2 -> minabs [e1,e2]) (f^.voiceEnvs)
