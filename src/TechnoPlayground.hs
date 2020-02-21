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
-- TODO: Add ability to do effects too.
-- TODO: Separate envelopes for arps and drums
-- TODO: Fix performance issues. Something to do with stitching segments together.

data TechnoGenerator = TechnoGenerator { _drumPatterns :: [Seg Sig2]
                                       , _arps :: [[Pch]]
                                       , _leads :: [[Pch]]
                                       , _envelopes :: [Sig]
                                       , _durations :: [Sig]
                                       , _instruments :: [Patch2]
                                       }
makeLenses ''TechnoGenerator

data TechnoState = TechnoState { _drumSelection :: Seg Sig2
                               , _arpSelection :: [Pch]
                               , _leadSelection :: [Pch]
                               , _envelopeSelection :: Sig
                               , _durationSelection :: Sig
                               , _instrumentSelection :: Patch2
                               }
makeLenses ''TechnoState

data TechnoChangeable = ChangeDrum
                      | ChangeArp
                      | ChangeLead
                      | ChangeEnvelope
                      | ChangeInstrument
                      deriving (Enum, Bounded)

randEnum :: (Enum a, Bounded a, MonadIO m) => m a
randEnum = randomFrom [minBound]

-- Change only a single part of the state.
changeOne :: (MonadIO m) => TechnoGenerator -> TechnoState -> m TechnoState
changeOne tg ts = do
  changeable <- randEnum
  case changeable of
    ChangeDrum -> change drumSelection drumPatterns
    ChangeArp -> change arpSelection arps
    ChangeLead -> change leadSelection leads
    ChangeEnvelope -> change envelopeSelection envelopes
    ChangeInstrument -> change instrumentSelection instruments
  where
    change selection from = do
      new <- liftIO $ randomFrom (tg^.from)
      return $ ts & selection .~ new

-- Generate n changeOne states.
-- Each state changes only one thing about the last state
generateNChangeTechnoStates :: (MonadIO m)
                              => TechnoGenerator
                              -> Int
                              -> Int
                              -> m [TechnoState]
generateNChangeTechnoStates tg nStates nChanges =
  go (nStates-1) [] =<< generateTechnoState tg
  where
    go 0 acc last = return $ acc ++ [last]
    go n acc last = go (n-1) (acc ++ [last]) =<< doNM nChanges (changeOne tg) last

randomFrom :: (MonadIO m) => [a] -> m a
randomFrom xs = do
  i <- liftIO $ randomRIO (0, length xs - 1)
  return $ xs !! i

generateTechnoState :: (MonadIO m) => TechnoGenerator -> m TechnoState
generateTechnoState tg = do
  drm <- liftIO $ randomFrom $ tg^.drumPatterns
  arp <- liftIO $ randomFrom $ tg^.arps
  lead <- liftIO $ randomFrom $ tg^.leads
  env <- liftIO $ randomFrom $ tg^.envelopes
  instrument <- liftIO $ randomFrom $ tg^.instruments
  duration <- liftIO $ randomFrom $ tg^.durations
  return TechnoState { _drumSelection=drm
                     , _arpSelection=arp
                     , _leadSelection=lead
                     , _envelopeSelection=env
                     , _durationSelection=duration
                     , _instrumentSelection=instrument
                     }
  
renderTechnoState :: TechnoState -> SongM
renderTechnoState ts = do
  arp <- compileI (ts^.instrumentSelection) (ts^.arpSelection)
  lead <- compileI (ts^.instrumentSelection) (ts^.leadSelection)
  forBeats
    (ts^.durationSelection)
    (har [ ts^.drumSelection
         , stereoMap (*(ts^.envelopeSelection)) <$> arp
         , lead
         ])

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

  let arp1 =
        [ Pch root 6 0.6 (1/2)
        , Pch (doN 3 succC root) 6 0.6 (1/2)
        , Pch (predC root) 6 0.6 (1/2)
        ]
      arp2 =
        [ Pch root 6 0.6 (1/2)
        , Pch (doN 6 succC root) 6 0.6 (1/2)
        , Pch (doN 4 succC root) 6 0.6 (1/2)
        ]
      arp3 =
        [ Pch root 6 0.6 (1/2)
        , Pch (predC root) 6 0.6 (1/2)
        , Silent 3
        ]
      arp4 =
        [ Pch root 6 0.6 (1/4)
        , Silent (1/4)
        ]
      arp5 = Pch <$> minorChord root ?? 7 ?? 0.6 ?? (1/3)

  let lead1 =
        ((Pch <$> majorScale root ?? 8 ?? 0.6 ?? (1/2)) !!)
        <$> [0, 3, 1, 2, 5, 2, 4, 3]

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
                                             , silence
                                             ]
                           , _arps = [ arp1
                                     , arp2
                                     , arp3
                                     , arp4
                                     , arp5
                                     , [Silent 1]
                                     ]
                           , _leads = [ lead1
                                      , [Silent 1]
                                      ]
                           , _instruments = [ simpleBass
                                            , epiano2
                                            , fmBass1
                                            , fmBass2
                                            , simpleBass
                                            , guitar
                                            , banyan
                                            ]
                           , _envelopes = [ constEnv
                                          , sqrEnv gBPM 0 1
                                          , sqrEnv gBPM 0 2
                                          , sqrEnv gBPM 0 4
                                          , sqrEnv gBPM 0 8
                                          ]
                           --, _durations = [16, 32, 64]
                           , _durations = [16]
                           }

  -- states <- generateNChangeTechnoStates tg 32 2
  states <- replicateM 32 $ generateTechnoState tg
  -- states <- pure <$> generateTechnoState tg
  sections <- traverse renderTechnoState states
  return $ loop (mel sections)

songEnv = SongEnv { _bpm=140
                  , _beatLength=1024
                  }

-- Okay weirdly, once we run once with the Mac options,
-- we then apparently get permanently enabled on mac.

-- The options to dacBy when using jabras with the mac
macJabraOpts :: Options
macJabraOpts =
  def
  <> setCoreAudio
  <> setRates 44100 10
  <> setBufs 512 1024
  <> setDac
  <> setAdc
  -- <> setInput "adc2"

dj :: (RenderCsd a) => a -> IO ()
dj = dacBy macJabraOpts

tec' = runSongM songEnv song
tec = dac =<< tec'
