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

env = SongEnv 128 32

song :: SongM
song = do
  kick <- drums "X _ _ _ _ _ _ _|O _ _ _ _ _ _ _|O _ _ _ _ _ _ _|O _ _ _ _ _ _ _" Mp.bd
  cows <- drums "O _ . _ . _ . _|_ _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|_ _ _ _ _ _ _ _" Mp.cow
  snar <- drums "_ _ _ _ _ _ o _|_ _ _ _ o _ _ _|_ _ o _ _ _ o _|_ _ _ _ O _ _ _" Mp.sn1
  sna2 <- drums "_ _ o _ o _ _ _|_ _ O _ _ _ _ _|_ _ _ _ X _ o _|_ _ O _ _ _ X _" Mp.sn2
  cyms <- drums ". _ . _ . _ . _|. . . . . . . .|. . . . . . . .|. . . . . . . ." Mp.cym1
  cymt <- drums "X _ o _ o _ o _|O _ o _ o _ o _|X _ o _ o _ o _|X _ o _ o _ o _" Mp.cym2
  tams <- drums "_ o _ X _ o _ X|_ _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|_ _ _ _ _ _ _ _" Mp.tam
  gros <- drums "_ _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|X _ _ _ _ _ _ _|X _ _ _ _ _ _ _" Mp.gro
  mars <- drums "_ _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|X _ _ _ X _ _ _|X _ _ _ X _ _ _" Mp.mar
  qujs <- drums "_ _ o _ _ _ _ _|_ _ o _ _ _ _ _|_ _ o _ _ _ _ _|_ _ o _ _ _ _ _" Mp.qj
  pad <- loop <$> compileI razorPad (Pch <$> (take 4 . cycle $ minorChord C) ?? 5 ?? 0.3 ?? 8)
  lead <- loop <$> compileI polySynth (take 64 $ cycle [Pch C 6 0.4 0.5, Silent 0.5])
  let intro1 = har [kick, cows]
      intro2 = har [kick, cows, snar]
      maindrum = har [kick, cows, snar, mars, tams, sna2]
  cotraverse (loop . mel)
    [ withDrop 4 12 qujs =<< forBeats 16 (intro1 =:= pad)
    , forBeats 16 intro2
    -- TODO: Why does this only play for 8??? Seems like it takes the lowest
    , forBeats 32 (maindrum =:= pad =:= lead)
    ]

sat = runSongM env song
