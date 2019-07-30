module Scratch (scratchSong) where

import Csound.Base

kick = osc (100 * linloop [1, 0.1 * takt 1, 0, 0.9 * takt 1, 0])
snare = at (hp 500 23) $ mul (sqrSeq [0, 0, 1, 0, 0, 0, 0.5, 0.2] $ syn 4) pink
hiHat = at (mlp 2500 0.1) $ mul (sawSeq [1, 0.5, 0.2, 0.5, 1, 0, 0, 0.5] $ syn 4) white
ticks = mul (sqrSeq [0, 0, 0, 0, 1, 1] $ syn 8) $ osc 440
scratchSong = fromMono <$> mul 0.3 (return (mul 2.4 kick) + return Scratch.ticks + snare + hiHat)
