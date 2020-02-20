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

-- TODO: Pre-load every section to avoid pauses
-- TODO: Envelope application doesn't seem to be working. Beats versus bars?
-- TODO: Add ability to do effects too.
-- TODO: Separate instrument from arp pattern so we can mix and match
-- TODO: Separate envelopes for arps and drums

data TechnoGenerator = TechnoGenerator { _drumPatterns :: [Seg Sig2]
                                       , _arps :: [Seg Sig2]
                                       , _envelopes :: [Sig]
                                       , _durations :: [Sig]
                                       }
makeLenses ''TechnoGenerator

data TechnoState = TechnoState { _drumSelection :: Seg Sig2
                               , _arpSelection :: Seg Sig2
                               , _envelopeSelection :: Sig
                               , _durationSelection :: Sig
                               }
makeLenses ''TechnoState

data TechnoChangeable = ChangeDrum
                      | ChangeArp
                      | ChangeEnvelope
                      | ChangeEffect

-- Change only a single part of the state.
changeOne :: (MonadIO m) => TechnoGenerator -> TechnoState -> m TechnoState
changeOne tg ts = do
  changeable <- randomFrom [ChangeDrum, ChangeArp, ChangeEnvelope]
  case changeable of
    ChangeDrum -> change drumSelection drumPatterns
    ChangeArp -> change arpSelection arps
    ChangeEnvelope -> change envelopeSelection envelopes
  where
    change selection from = do
      new <- liftIO $ randomFrom (tg^.from)
      return $ ts & selection .~ new

-- Generate n changeOne states.
generateOneChangeTechnoStates :: (MonadIO m)
                              => TechnoGenerator
                              -> Int
                              -> m [TechnoState]
generateOneChangeTechnoStates tg n =
  go (n-1) [] =<< generateTechnoState tg
  where
    go 0 acc last = return $ acc ++ [last]
    go n acc last = go (n-1) (acc ++ [last]) =<< changeOne tg last

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
    (har [ts^.drumSelection, stereoMap (*(ts^.envelopeSelection)) <$> (ts^.arpSelection)])

root = D

song :: SongM
song = do
  kcks1 <- drums "O _ _ _ _ _ _ _|o _ _ _ _ _ _ _|O _ _ _ _ _ _ _|o _ _ _ _ _ _ _|" Tr808.bd2
  snrs1 <- drums "_ _ _ _ _ _ _ _|X _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|X _ _ _ _ _ _ _|" Tr808.sn
  chhs1 <- drums "_ _ _ _ o _ _ _|_ _ _ _ . _ _ _|_ _ _ _ o _ _ _|_ _ _ _ . _ _ _|" Tr808.chh
  chhs2 <- drums "O _ o _ _ _ o _|" Tr808.chh
  ohhs1 <- drums "o _ _ _ o _ _ _|" Tr808.ohh
  ohhs2 <- drums "_ _ _ _ O _ _ _|" Tr808.ohh
  cyms1 <- drums "O _ o _ . _ . _|" Tr808.cym
  clps1 <- drums "_ _ _ _ _ _ _ _|X _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|X _ _ _ _ _ _ _|" Hm.clap

  let pat0 = har [kcks1]
      pat1 = har [kcks1, clps1]
      pat2 = har [kcks1, clps1, chhs1]
      pat3 = har [kcks1, clps1, ohhs1]
      pat4 = har [kcks1, clps1, cyms1]
      pat5 = har [kcks1, chhs2, ohhs2]
      pat6 = har [kcks1, chhs2, ohhs2, clps1]
      pat7 = har [kcks1, snrs1]
      pat8 = har [chhs1, ohhs1]

  bass1 <-
    compileI epiano2
    [ Pch root 6 0.8 (1/2)
    , Pch (doN 3 succC root) 6 0.8 (1/2)
    , Pch (predC root) 6 0.8 (1/2)
    ]

  bass2 <-
    compileI epiano2
    [ Pch root 6 0.8 (1/2)
    , Pch (doN 6 succC root) 6 0.8 (1/2)
    , Pch (doN 4 succC root) 6 0.8 (1/2)
    ]

  bass3 <-
    compileI epiano2
    [ Pch root 6 0.8 (1/2)
    , Pch (predC root) 6 0.8 (1/2)
    , Silent 3
    ]

  bass4 <- compileI epiano2 [ Pch root 6 0.8 (1/4)
                            , Silent (1/4)
                            ]

  -- TODO: Remove BPM ask with MonadReader refactor.
  gBPM <- asks (view bpm)
  let silence = restSig (Beats gBPM 4)

  let tg = TechnoGenerator { _drumPatterns = [ pat0
                                             , pat1
                                             , pat2
                                             , pat3
                                             , pat4
                                             , pat5
                                             , pat6
                                             , pat7
                                             , pat8
                                             --, silence
                                             ]
                           , _arps = [ bass1
                                     , bass2
                                     , bass3
                                     , bass4
                                     --, silence
                                     ]
                           , _envelopes = [ constEnv
                                          , sqrEnv gBPM 0 1
                                          , sqrEnv gBPM 0 2
                                          , sqrEnv gBPM 0 4
                                          , sqrEnv gBPM 0 8
                                          ]
                           , _durations = [16, 32, 64]
                           }

  -- states <- generateOneChangeTechnoStates tg 4
  states <- replicateM 16 $ generateTechnoState tg
  sections <- traverse renderTechnoState states
  return $ loop (mel sections)

songEnv = SongEnv { _bpm=140
                  , _beatLength=512
                  }
tec' = runSongM songEnv song
tec = dac =<< tec'
