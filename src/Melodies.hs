module Melodies (twinkle) where

import Csound.Base
import Csound.Sam
import Note

-- TODO: Coherent way of sequencing notes that isn't float numbers

p1 a b = mel $ fmap temp [a, a, b, b]
p2 a b = mel [mel $ fmap temp [a, a], str 2 $ temp b]
p3 a b c d = mel [p1 a b, p2 c d]

ph1 = p3 8.00 8.07 8.09 8.07
ph2 = p3 8.05 8.04 8.02 8.00
ph3 = p3 8.07 8.05 8.04 8.02

ph12 = mel [ph1, ph2]
ph33 = loopBy 2 ph3

twinkle :: Track Sig D
twinkle = mel [ph12, ph33, ph12]
