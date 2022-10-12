{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}

module Sketches where

import Control.Lens
import Control.Monad.Random
import Control.Monad.Reader
import Csound.Air.Filter (clp)
import Csound.Base hiding (Duration, Tab, clp, pulse, sqr, tri)
import Csound.Catalog.Drum.Hm (clap)
import qualified Csound.Catalog.Drum.Hm as Hm
import Csound.Catalog.Drum.Tr808 hiding (bass)
import Csound.Catalog.Effect
--https://hackage.haskell.org/package/csound-catalog-0.7.6/docs/Csound-Patch.html
--https://hackage.haskell.org/package/csound-catalog-0.7.6/docs/Csound-Patch.html
--https://hackage.haskell.org/package/csound-catalog-0.7.6/docs/Csound-Patch.html
--https://hackage.haskell.org/package/csound-catalog-0.7.6/docs/Csound-Patch.html
--https://hackage.haskell.org/package/csound-catalog-0.7.6/docs/Csound-Patch.html
--https://hackage.haskell.org/package/csound-catalog-0.7.6/docs/Csound-Patch.html
--https://hackage.haskell.org/package/csound-catalog-0.7.6/docs/Csound-Patch.html
--https://hackage.haskell.org/package/csound-catalog-0.7.6/docs/Csound-Patch.html
import Csound.Patch
  ( cathedralOrgan,
    dreamPad,
    epiano1,
    epianoBright,
    mutedPiano,
    pwBass,
    razorLead,
    simpleMarimba,
    underwaterPad,
    waveOrgan,
    withDeepBass,
  )
import Csound.Sam
import Data.List
import Data.Ord
import Data.Ratio ((%))
import Data.Sort
import Melody
import Note
import System.IO.Unsafe
import System.Random
import Tabs
import Tools

n octave dur note = Pch note octave 0.5 dur

play bpm beats s = dac =<< runSongM (SongEnv bpm beats) (har <$> s)

record bpm beats s = runToDisk =<< runSongM (SongEnv bpm beats) (har <$> s)

muted = compileI mutedPiano

epianoB = compileI epianoBright

epiano = compileI epiano1

bass = compileI (withDeepBass 0.75 pwBass)

organ = compileI (deepPad cathedralOrgan)

razor = compileI (deepPad razorLead)

marim = compileI simpleMarimba

sqr :: MonadReader SongEnv m => [Pch] -> m (Seg Sig2)
sqr = compileI $ waveOrgan rndSqr

tri :: MonadReader SongEnv m => [Pch] -> m (Seg Sig2)
tri = compileI $ waveOrgan rndTri

saw :: MonadReader SongEnv m => [Pch] -> m (Seg Sig2)
saw = compileI $ waveOrgan rndSaw

pulse :: MonadReader SongEnv m => [Pch] -> m (Seg Sig2)
pulse = compileI $ waveOrgan rndPulse

upad = compileI underwaterPad

--dpad = compileI dreamPad

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
      epianoB (n <$> [7, 8] <*> pure 1 <*> minorChord Eb) <**> (withEnv <$> sqrEnvM (1 / 5) 4),
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
              [epianoB (drop 2 $ ns 8 root minorScale)],
              [organ (ns 6 root minorScale), hats, cyms]
            ]
    )

sketch3 :: IO ()
sketch3 = play 140 256 do
  let arps root =
        [ sort $ expandScale [6 .. 9] (minorChord root) ?? 0.3 ?? 0.5,
          sort $ expandScale [8 .. 10] (minorChord (doN 5 succC root)) ?? 0.3 ?? 0.75,
          sortOn Down $ expandScale [5, 6] (minorChord (doN 10 succC root)) <*> [0.3, 0.4] ?? 0.25,
          sortOn Down $ expandScale [8, 10] (minorScale root) <*> [0.3, 0.2] ?? 1,
          sort $ expandScale [8] (majorChord (doN 3 succC root)) ?? 0.7 ?? 3
        ]
  envs <- sequence (sinEnvM <$+> [0, 0.2, 0.4, 0.6, 0.8] <*++> [1, 2, 3, 4, 5])
  toSeg . stereoMap (equalizer [(0.1, 0.1)] 0.1) . runSeg <$$> zipEnvs envs (epiano <$> arps Fs)

sketch3b :: IO ()
sketch3b = play 140 256 do
  let arps root =
        [ sort $ expandScale [6 .. 9] (minorChord root) ?? 0.3 ?? 0.5,
          sort $ expandScale [8 .. 10] (minorChord (doN 5 succC root)) ?? 0.3 ?? 0.75,
          sortOn Down $ expandScale [5, 6] (minorChord (doN 10 succC root)) <*> [0.3, 0.4] ?? 0.25,
          sortOn Down $ expandScale [8, 10] (minorScale root) <*> [0.3, 0.2] ?? 1,
          sort $ expandScale [8] (majorChord (doN 3 succC root)) ?? 0.7 ?? 3
        ]
  envs <- sequence (sinEnvM <$+> [0, 0.2, 0.4, 0.6, 0.8] <*++> [1, 2, 3, 4, 5])
  rest <- toSeg . stereoMap (equalizer [(0.1, 0.1)] 0.1) . runSeg <$$> zipEnvs envs (epiano <$> arps Fs)
  kcks <- drums "X" bd2
  snrs <- drumsDrop "_ _ o |_ _ O _|_ _ O _ |X _ X X|" clap 0.2 4
  hats <- drumsDrop "O o o .|" chh 0.1 8
  return $ kcks : snrs : hats : rest

sketch4 = play 140 128 do
  let ns root i off = take i . drop off . cycle $ n 7 (2 / fromIntegral i) <$> minorScale root
      s ins root i off = ins $ ns root i off
      ss ins is os root = s ins root <$> is <*> os
      os = concat . transpose $ (ss bass [2, 1] [0, 2, 1] <$> minorChord C) ++ (ss marim [3, 6, 2] [1, 0] <$> minorChord C)
      voices = fromIntegral $ length os
  es <- sequence $ sqrTabEnv <$> [[OffFor (i * 8), OnFor (4 * 8), OffFor ((voices - 4 - i) * 8)] | i <- [0 .. voices - 1]]
  dre <- sqrTabEnv [OnFor 12, OffFor 4]
  drms <-
    har
      <$> sequence
        [ drums "X|o|o|o|" bd2 <&> withEnv dre,
          drumsDr (concat $ replicate 4 "_|X|_|O _ X X|") clap 0.3,
          drumsDr (concat $ replicate 4 "X O o .|X O o o . .|") chh 0.2,
          drums "_ | _ _ O _" ohh
        ]
  let drms' = smallRoom2 . stereoMap (fxCompress 1.0 (0.1, 0.9) 1.0 (0.1, 0.9) 1.0) <$> drms
  pure . (drms' =:=) . har <$> zipEnvs es os

sketch5 :: IO ()
sketch5 = play 150 256 do
  let arp ns pat = (ns !!) <$> pat
      a1 = arp (minorChord C) [0, 0, 2, 1, 1, 0, 2, 1]
      a2 = arp (major7Chord Eb) [2, 1, 3, 0, 3, 2, 0, 1]
      a3 = arp (minor7Chord G) [3, 0, 1, 0, 2, 0, 1, 0]
  arps <- tri (n 8 (1 / 4) <$> (a1 ++ a2 ++ a3 ++ a2))
  let b1 = n 5 4 <$> [C, Bb, Eb, Fs]
      b2 = n 6 4 <$> [C, Fs, Eb, Bb]
  bassE <- sqrTabEnv [OffFor 32, OnFor 64]
  bass <- withEnv bassE <$> sqr (concat [b1, b1, b1, b2])
  es <- sequence (sqrEnvM <$+> [0, 0.25, 0.5, 0.75] <*++> replicate 4 16)
  kcks <- drums "X" bd2
  chhs <- drums "O o . . | _ _ X _" chh
  ohhs <- drums "_ _ X _ | O o _ o" ohh
  clps <- drums (concat (replicate 3 "_ | X | ") ++ "_ | X X X X") clap
  let drms = har $ withEnv <$+> es <*++> [kcks, chhs, ohhs, clps]
  return
    [ stereoMap (fxLoFi 0.2 0.2) <$> drms,
      fxTrem 1 (1 / 7) (3 / 4) <$> bass,
      arps
    ]

bit8 :: Sig2 -> Sig2
bit8 = stereoMap (fxLoFi 0.2 0.2)

data NoteTab
  = S
  | S'
  | S''
  | S'''
  | S_
  | S__
  | S___
  | N Note Octave
  | N' Note Octave
  | N'' Note Octave
  | N''' Note Octave
  | N_ Note Octave
  | N__ Note Octave
  | N___ Note Octave

noteTab :: Velocity -> NoteTab -> Pch
noteTab _ S = Silent 1
noteTab _ S' = Silent (1 / 2)
noteTab _ S'' = Silent (1 / 4)
noteTab _ S''' = Silent (1 / 8)
noteTab _ S_ = Silent 2
noteTab _ S__ = Silent 4
noteTab _ S___ = Silent 8
noteTab v (N n o) = Pch n o v 1
noteTab v (N' n o) = Pch n o v (1 / 2)
noteTab v (N'' n o) = Pch n o v (1 / 4)
noteTab v (N''' n o) = Pch n o v (1 / 8)
noteTab v (N_ n o) = Pch n o v 2
noteTab v (N__ n o) = Pch n o v 4
noteTab v (N___ n o) = Pch n o v 8

sketch6 :: IO ()
sketch6 = play 140 256 do
  kcks <- bit8 <$$> drums "X" bd2
  snrs <- bit8 <$$> drums "_|X" clap
  hats <- bit8 <$$> drums "_ X" chh
  let b1 = n 6 4 <$> [Eb, F, Ab, Bb]
      b2 = n 7 4 <$> [Eb, Bb, Ab, G]
  b <- bit8 . fxTrem 0 0.14 0.3 <$$> sqr (concat [b1, b1, b1, b2])
  let n1 = noteTab 1 <$> [N Eb 8, S, N' G 8, N'' Ab 8, N'' G 8, N F 8]
      n2 = noteTab 1 <$> [N Eb 9, S, N' Bb 8, N'' Ab 8, N'' F 8, N'' Ab 8, N'' F 8, N'' G 8, N'' D 8]
  m <- bit8 <$$> tri (n1 ++ n2 ++ reverse n1 ++ reverse n2)
  return [kcks, snrs, hats, m]

sketch7 :: IO ()
sketch7 = play 120 256 do
  let surround i v a b ns = i $ noteTab v <$> replicate a S ++ ns ++ replicate b S
  m1 <- surround muted 0.03 0 27 [N''' F 8, N''' G 8, N''' Bb 8, N''' D 9]
  m2 <- surround muted 0.03 7 20 [N F 8, N G 8, N Bb 8, N D 9]
  m3 <- surround muted 0.03 17 17 [N Bb 7, S, S, N'' G 7, N'' D 7, S''', N F 7]
  m4 <- surround muted 0.03 48 3 [N A 9, S, N F 9, S'', S', S''', N''' F 9, N G 9]
  m5 <- surround muted 0.05 61 19 [N__ F 10, N__ Bb 10, N___ G 10, N___ Bb 10]
  m6 <- surround upad 0.4 12 41 [N__ F 8, S, N_ F 8, N__ Bb 8, S, N__ G 8, N___ F 8]
  kcks <- withEnv <$> sqrTabEnv [OffFor 56, OnFor 32] <*> drums "o|_|_|_" bd2
  return $ smallHall2 <$$> [kcks, m1, m2, m3, m4, m5, m6]
