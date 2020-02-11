{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

compileD = compileTabs gBPM . pure
kick = compileD $ DrumTab "X _ _ _ _ _ _ _|O _ _ _ _ _ _ _|O _ _ _ _ _ _ _|O _ _ _ _ _ _ _" Mp.bd numBeats
cows = compileD $ DrumTab "O _ . _ . _ . _|_ _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|_ _ _ _ _ _ _ _" Mp.cow numBeats
snar = compileD $ DrumTab "_ _ _ _ _ _ o _|_ _ _ _ o _ _ _|_ _ o _ _ _ o _|_ _ _ _ O _ _ _" Mp.sn1 numBeats
sna2 = compileD $ DrumTab "_ _ o _ o _ _ _|_ _ O _ _ _ _ _|_ _ _ _ X _ o _|_ _ O _ _ _ X _" Mp.sn2 numBeats
cyms = compileD $ DrumTab ". _ . _ . _ . _|. . . . . . . .|. . . . . . . .|. . . . . . . ." Mp.cym1 numBeats
cymt = compileD $ DrumTab "X _ o _ o _ o _|O _ o _ o _ o _|X _ o _ o _ o _|X _ o _ o _ o _" Mp.cym2 numBeats
tams = compileD $ DrumTab "_ o _ X _ o _ X|_ _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|_ _ _ _ _ _ _ _" Mp.tam numBeats
gros = compileD $ DrumTab "_ _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|X _ _ _ _ _ _ _|X _ _ _ _ _ _ _" Mp.gro numBeats
mars = compileD $ DrumTab "_ _ _ _ _ _ _ _|_ _ _ _ _ _ _ _|X _ _ _ X _ _ _|X _ _ _ X _ _ _" Mp.mar numBeats
qujs = compileD $ DrumTab "_ _ o _ _ _ _ _|_ _ o _ _ _ _ _|_ _ o _ _ _ _ _|_ _ o _ _ _ _ _" Mp.qj numBeats

padNotes = Pch <$> (take 4 . cycle $ minorChord C) ?? 5 ?? 0.3 ?? 8
pad = compileWith (Env gBPM razorPad $ fromIntegral numBeats) padNotes
lead = compileWith (Env gBPM polySynth $ fromIntegral numBeats) (take 64 $ cycle [Pch C 6 0.4 0.5, Silent 0.5])

-- TODO: Figure out MTL stack with a ReaderT here
-- Maybe need a liftSE

newtype SongM a = SongM {
  runSong :: ReaderT Bpm SE a
} deriving (Monad, Functor, Applicative) -- , MonadIO, MonadError AppError)

song :: SE (Seg Sig2)
song = do
  drop <- forBeats gBPM 4 . toSeg <$> qujs
  intro1 <- forBeats gBPM 16 . toSeg <$> sum [kick, cows]
  intro2 <- forBeats gBPM 16 . toSeg <$> sum [kick, cows, snar]
  maindrum <- forBeats gBPM 32 . toSeg <$> sum [kick, cows, snar, mars, tams, sna2]
  mainlead <- forBeats gBPM 32 . toSeg <$> pure lead
  mainpad <- forBeats gBPM 32 . toSeg <$> pure pad
  return
    $ loop . mel
    $ [ intro1 =:= mainpad & withDrop gBPM 4 12 drop
      , intro2
      , maindrum =:= mainpad
      ]

sat = runB gBPM (runSeg <$> song)
