{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}

module Sketches where

import Control.Lens
import Control.Monad.Random
import Control.Monad.Reader
import Csound.Base hiding (Duration, Tab, clp)
import qualified Csound.Catalog.Drum.Hm as Hm
import Csound.Catalog.Drum.Tr808 hiding (bass)
import Csound.Catalog.Effect
import Csound.Patch
import Csound.Sam
import Data.List
import Data.Ord
import Data.Sort
import Melody
import Note
import System.IO.Unsafe
import System.Random
import Tabs
import Tools
  ( Beats (Beats),
    SongEnv (SongEnv),
    cotraverse,
    forBeats,
    limSig,
    runSongM,
    runToDisk,
    (<$$>),
    (<$+>),
    (<*++>),
  )

n octave dur note = Pch note octave 0.5 dur

play bpm beats s = dac =<< runSongM (SongEnv bpm beats) (har <$> s)

record bpm beats s = runToDisk =<< runSongM (SongEnv bpm beats) (har <$> s)

piano = compileI epianoBright

bass = compileI (withDeepBass 0.75 pwBass)

organ = compileI (deepPad cathedralOrgan)

razor = compileI (deepPad razorLead)

zipEnvs es xs' =
  do
    xs <- sequence xs'
    return [withEnv e x | (e, x) <- zip es xs]

sketch1 :: IO ()
sketch1 = play 128 128 do
  es <-
    sequence $
      sqrEnvM
        <$+> [0.5, 1 / 2, 1 / 4, 3 / 4, 1 / 8, 0, 1 / 2]
        <*++> [16, 24, 12, 32, 16, 1 / 4, 4]
  zipEnvs
    es
    [ drums "X|o|o|o|" bd2,
      drums "_|X" sn1,
      drums "O o _ .|" chh,
      drums "_ _ X _|" ohh,
      drums "_|_|_|. o O X|" cym,
      piano (n <$> [7, 8] <*> pure 1 <*> minorChord Eb) <**> (withEnv <$> sqrEnvM (1 / 5) 4),
      bass $ n 6 1 <$> (take 5 . cycle . reverse . minorChord =<< [Ab, Eb])
    ]

silence = Silent 1

sketch2 :: IO ()
sketch2 = play 140 128 do
  let root = Bb
      ns o r s =
        intercalate
          [n o (1 / 4) r, silence, n o (1 / 4) r]
          (pure . n 8 (1 / 2) <$> take 7 (cycle . reverse $ s r))
      kcks = drums "X|o|o|o|" bd2
      hats = drums "O o o .|" chh
      snrs = drums "_|X|_|X O _ _" sn1
      cyms = drums "_|_|_|X" cym
  es <- sequence $ sqrEnvM <$+> [0, 0.5, 0.25, 0.5, 0.75] <*++> replicate 5 32
  zipEnvs
    es
    ( fmap har . sequence
        <$> [ [kcks, hats],
              [kcks, snrs],
              [razor $ ns 7 root minorScale, cyms],
              [piano (drop 2 $ ns 8 root minorScale)],
              [organ (ns 6 root minorScale), hats, cyms]
            ]
    )
