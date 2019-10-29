module Tetris where

import Csound.Base hiding (Tab)
import qualified Csound.Catalog.Drum.Tr808 as Tr808
import qualified Csound.Catalog.Drum.Hm as Hm
import Csound.Patch
import Csound.Sam
import Csound.Sam.Core
import Data.List.Split
import Tabs
import Tools
import Note
import Melody
import FirstSong

-- TODO: Some chords for Tetris to make things sounds a lil nicer.
-- Plus compression effects for an 8-bit sound.

tetrisNotes1 :: [Pch]
tetrisNotes1 =
  getZipList $
  Pch <$>
  ZipList [E, B, C, D, C, B, A, C, E, D, C, B, B, C, D, E, C, A, A] <*>
  ZipList [8, 7, 8, 8, 8, 7, 7, 8, 8, 8, 8, 7, 7, 8, 8, 8, 8, 7, 7] <*>
  ZipList (repeat 1.0) <*>
  ZipList ([2, 1, 1, 2, 1, 1, 3, 1, 2, 1, 1, 2, 1, 1, 2, 2, 2, 2, 5] <**> pure (*0.25))

tetrisNotes2 :: [Pch]
tetrisNotes2 =
  getZipList $
  Pch <$>
  ZipList [D, F, A, G, F, E, C, E, D, C, B, B, C, D, E, C, A, A]  <*>
  ZipList [8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 7, 7, 8, 8, 8, 8, 7, 7] <*>
  ZipList (repeat 1.0) <*>
  ZipList ([2, 1, 2, 1, 1, 3, 1, 2, 1, 1, 2, 1, 1, 2, 2, 2, 2, 4] <**> pure (*0.25))

tetrisNotes3 :: [Pch]
tetrisNotes3 =
  getZipList $
  Pch <$>
  ZipList [E, C, D, B, C, A, Ab, E, C, D, B, C, E, A, A, Ab]  <*>
  ZipList [8, 8, 8, 7, 8, 7, 7, 8, 8, 8, 7, 8, 8, 8, 8, 8] <*>
  ZipList (repeat 1.0) <*>
  ZipList ([4, 4, 4, 4, 4, 4, 8, 4, 4, 4, 4, 2, 2, 2, 2, 8] <**> pure (*0.25))

tetrisNotes = concat [tetrisNotes1, tetrisNotes2, tetrisNotes1, tetrisNotes2, tetrisNotes3]
tetrisLead = compileSegment $ Segment houseBpm overtoneLead $ loopBy 32 $ toMel tetrisNotes

-- TODO: Unfinished - only have the first bar.
tetrisBassNotes :: [Pch]
tetrisBassNotes = allNotes
  where
    notes1 = dup 4 [Pch C 7 1.0 (1/4), Pch E 7 1.0 (1/4)]
    notes2 = dup 4 [Pch A 6 1.0 (1/4), Pch C 7 1.0 (1/4)]
    notes3 = dup 4 [Pch Ab 6 1.0 (1/4), Pch B 6 1.0 (1/4)] 
    notes4 = dup 4 [Pch A 6 1.0 (1/4), Pch C 7 1.0 (1/4)]
    allNotes = concat [notes1, notes2, notes3, notes4]

-- TODO: Compile with different instrument
tetrisBass = compileSegment $ Segment houseBpm overtoneLead $ toMel tetrisBassNotes

tetrisHouseSong = sum [head houseDrums, inOutFilter $ pure tetrisLead, pure tetrisBass]
tetrisDnbSong = sum [
    dnbDrums
  , pure $ compileSegment $ Segment (bpm/2) razorLead $ loopBy 8 $ toMel tetrisNotes
  , pure $ compileSegment $ Segment (bpm/2) overtoneLead $ loopBy 8 $ toMel tetrisBassNotes
                    ]
