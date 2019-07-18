module Note where

import Csound.Base

data Note = C | Cs | Db | D | Ds | Eb | E | F | Fs | Gb | G | Gs | Ab | A | As | Bb | B
type Octave = Int
data Pch = Pch Note Octave

toIndex :: Note -> Int
toIndex C = 0
toIndex Cs = 1
toIndex Db = 1
toIndex D = 2
toIndex Ds = 3
toIndex Eb = 3
toIndex E = 4
toIndex F = 5
toIndex Fs = 6
toIndex Gb = 6
toIndex G = 7
toIndex Gs = 8
toIndex Ab = 8
toIndex A = 9
toIndex As = 10
toIndex Bb = 10
toIndex B = 11

-- Converts Pch note to its corresponding D e.g. 8.00 for mid-C
toD :: Pch -> D
toD (Pch n o) = octaveN + noteN
  where
    octaveN = fromIntegral o
    noteN = 0.01 * fromIntegral (toIndex n)

-- TODO: Respect velocity.
toDD :: Pch -> (D, D)
toDD p = (0.5, toD p)

-- Converts notes to temps
toTemps :: [Pch] -> [Track Sig (D, D)]
toTemps pchs = temp . toDD <$> pchs

-- Converts notes to melody
toMel :: [Pch] -> Track Sig (D, D)
toMel = mel . toTemps

-- Converts notes to chord
toChord :: [Pch] -> Track Sig (D, D)
toChord = har . toTemps
