{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module TechnoPlayground where

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

-- TODO here:
-- randomised section generator that spits out
-- length, arptype, envelope mix, effect mix
--
-- For later: slow right down, introduce noise, and make
-- binaural / infinite ambient generator

data TechnoGenerator = TechnoGenerator { _drumPatterns :: [Seg Sig2]
                                       , _arps :: [Seg Sig2]
                                       , _envelopes :: [Sig]
                                       -- The beat durations to choose from.
                                       -- Will always change this.
                                       , _durations :: [Sig]
                                       }
makeLenses ''TechnoGenerator

data TechnoState = TechnoState { _drumSelection :: Seg Sig2
                               , _arpSelection :: Seg Sig2
                               , _envelopeSelection :: Sig
                               , _durationSelection :: Sig
                               }
makeLenses ''TechnoState

data TechnoChangeable = ChangeDrums
                      | ChangeArps
                      | ChangeEnvelope

randomFrom :: (MonadIO m) => [a] -> m a
randomFrom xs = do
  i <- liftIO $ randomRIO (0, length xs - 1)
  return $ xs !! i

generateTechnoState :: (MonadIO m) => TechnoGenerator -> m TechnoState
generateTechnoState tg = do
  drm <- liftIO $ randomFrom $ tg^.drumPatterns
  arp <- liftIO $ randomFrom $ tg^.arps
  env <- liftIO $ randomFrom $ tg^.envelopes
  duration <- liftIO $ randomFrom $ tg^.durations
  return TechnoState { _drumSelection=drm
                     , _arpSelection=arp
                     , _envelopeSelection=env
                     , _durationSelection=duration
                     }
  
renderTechnoState :: TechnoState -> SongM
renderTechnoState ts =
  forBeats
    (ts^.durationSelection)
    (fmap (stereoMap (*(ts^.envelopeSelection))) (har [ts^.drumSelection, ts^.arpSelection]))

root = D

song :: SongM
song = do
  kcks1 <- drums "O _ _ _ _ _ _ _|o _ _ _ _ _ _ _|O _ _ _ _ _ _ _|o _ _ _ _ _ _ _|" Tr808.bd2
  snrs1 <- drums "_ _ _ _ _ _ _ _|X _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|X _ _ _ _ _ _ _|" Tr808.sn
  chhs1 <- drums "_ _ _ _ o _ _ _|_ _ _ _ . _ _ _|_ _ _ _ o _ _ _|_ _ _ _ . _ _ _|" Tr808.chh
  ohhs1 <- drums "o _ _ _ o _ _ _|" Tr808.ohh
  cyms1 <- drums "O _ o _ . _ . _|" Tr808.cym
  clps1 <- drums "_ _ _ _ _ _ _ _|X _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|X _ _ _ _ _ _ _|" Hm.clap

  let pat0 = har [kcks1]
      pat1 = har [kcks1, clps1]
      pat2 = har [kcks1, clps1, chhs1]
      pat3 = har [kcks1, clps1, ohhs1]
      pat4 = har [kcks1, clps1, cyms1]

  pad <-
    -- TODO: rename to e.g. instr
    compileI dreamPad
    [ Pch root 6 0.8 8
    , Silent 8
    ]

  bass <-
    compileI fmBass2
    [ Pch root 6 0.8 (1/2)
    , Pch (doN 3 succC root) 6 0.8 (1/2)
    , Pch (predC root) 6 0.8 (1/2)
    ]

  gBPM <- asks (view bpm)
  let tg = TechnoGenerator { _drumPatterns = [ pat0
                                             , pat1
                                             , pat2
                                             , pat3
                                             , pat4
                                             ]
                           , _arps = [ pad
                                     , bass
                                     ]
                           , _envelopes = [ constEnv
                                          , sinEnv gBPM 0 8
                                          , sinEnv gBPM 0 16
                                          , sinEnv gBPM 0 32
                                          ]
                           , _durations = [8, 16, 32]
                           }

  states <- replicateM 4 (generateTechnoState tg)
  sections <- sequence $ renderTechnoState <$> states
  return $ loop (mel sections)

songEnv = SongEnv { _bpm=140
                  , _beatLength=1024
                  }
tec' = runSongM songEnv song
tec = dac =<< tec'
