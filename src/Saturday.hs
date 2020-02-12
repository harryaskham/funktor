{-# LANGUAGE FlexibleContexts #-}

module Saturday where

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
import qualified Csound.Catalog.Drum as Drum
import Csound.Catalog.Drum.MiniPops as Mp
import Csound.Catalog.Effect
import System.IO.Unsafe
import Control.Monad.Reader

gBPM = 128
numBeats = 32

compileD' = fmap toSeg . compileTabs gBPM . pure
kick = compileD' $ DrumTab "X _ _ _ _ _ _ _|O _ _ _ _ _ _ _|O _ _ _ _ _ _ _|O _ _ _ _ _ _ _" Mp.bd
cows = compileD' $ DrumTab "O _ . _ . _ . _|_ _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|_ _ _ _ _ _ _ _" Mp.cow
snar = compileD' $ DrumTab "_ _ _ _ _ _ o _|_ _ _ _ o _ _ _|_ _ o _ _ _ o _|_ _ _ _ O _ _ _" Mp.sn1
sna2 = compileD' $ DrumTab "_ _ o _ o _ _ _|_ _ O _ _ _ _ _|_ _ _ _ X _ o _|_ _ O _ _ _ X _" Mp.sn2
cyms = compileD' $ DrumTab ". _ . _ . _ . _|. . . . . . . .|. . . . . . . .|. . . . . . . ." Mp.cym1
cymt = compileD' $ DrumTab "X _ o _ o _ o _|O _ o _ o _ o _|X _ o _ o _ o _|X _ o _ o _ o _" Mp.cym2
tams = compileD' $ DrumTab "_ o _ X _ o _ X|_ _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|_ _ _ _ _ _ _ _" Mp.tam
gros = compileD' $ DrumTab "_ _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|X _ _ _ _ _ _ _|X _ _ _ _ _ _ _" Mp.gro
mars = compileD' $ DrumTab "_ _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|X _ _ _ X _ _ _|X _ _ _ X _ _ _" Mp.mar
qujs = compileD' $ DrumTab "_ _ o _ _ _ _ _|_ _ o _ _ _ _ _|_ _ o _ _ _ _ _|_ _ o _ _ _ _ _" Mp.qj

padNotes = Pch <$> (take 4 . cycle $ minorChord C) ?? 5 ?? 0.3 ?? 8
pad = loop . toSeg $ compileWith
        (Env gBPM razorPad)
        padNotes
lead = loop . toSeg $ compileWith
         (Env gBPM polySynth)
         (take 64 $ cycle [Pch C 6 0.4 0.5, Silent 0.5])

song :: (MonadReader Bpm m, MonadSE m) => m (Seg Sig2)
song = do
  drop <- liftSE qujs
  intro1 <- liftSE (cotraverse har [kick, cows])
  intro2 <- liftSE (cotraverse har [kick, cows, snar])
  maindrum <- liftSE (cotraverse har [kick, cows, snar, mars, tams, sna2])
  cotraverse (loop . mel)
    [ withDrop 4 12 drop =<< forBeats 16 (intro1 =:= pad)
    , forBeats 16 intro2
    -- TODO: Why does this only play for 8??? Seems like it takes the lowest
    , forBeats 32 (maindrum =:= pad =:= lead)
    ]

sat = runB gBPM (runSeg <$> runReaderT (song :: SongM) gBPM)
