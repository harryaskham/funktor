module FirstSong where

import Csound.Base
import Csound.Catalog.Drum.Tr808
import Csound.Patch
import Csound.Sam

-- TODO: Once in a state to split up, do so into commented composable utils

-- Global controls + derivations
bpm = 140
bps = 140 / 60
spb = 1 / bps

kicks = pat' [1, 0.7, 0.9, 0.7] [4] bd
hats = del 3 $ pat' [1, 0.6] [3, 5] chh
snares = pat' [1, 0.5] [4] sn
drums = runSam (bpm * 4) $ kicks + hats + snares

p1 a b = mel $ fmap temp [a, a, b, b]
p2 a b = mel [mel $ fmap temp [a, a], str 2 $ temp b]
p3 a b c d = mel [p1 a b, p2 c d]

ph1 = p3 8.00 8.07 8.09 8.07
ph2 = p3 8.05 8.04 8.02 8.00
ph3 = p3 8.07 8.05 8.04 8.02

ph12 = mel [ph1, ph2]
ph33 = loopBy 2 ph3
ph   = mel [ph12, ph33, ph12]

-- Instrument defn
oscInstr :: D -> SE Sig
oscInstr x = return $ mul (linsegr [0, 0.03, 1, 0.2, 0] 0.1 0) $ osc $ sig x

-- Compiles the given track using the given instrument, ensuring BPM matches.
-- TODO: Extend to non-mono, non-simple instruments
compile :: (D -> SE Sig) -> Track Sig D -> Sig
compile instr = mix . sco instr . fmap cpspch . str spb

melody = fromMono . compile oscInstr $ ph

song = sum [pure melody, drums]
