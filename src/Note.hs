module Note where

import Csound.Base hiding (Duration)

data Note = C | Cs | Db | D | Ds | Eb | E | F | Fs | Gb | G | Gs | Ab | A | As | Bb | B
type Octave = Int
type Duration = Sig
type Velocity = D
data Pch = Pch Note Octave Velocity Duration | Silent Duration

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
toD (Pch n o v d) = octaveN + noteN
  where
    octaveN = fromIntegral o
    noteN = 0.01 * fromIntegral (toIndex n)
toD (Silent d) = error "wtf"

-- Converts to CsdNote type with velocity
toDD :: Pch -> (D, D)
toDD p@(Pch _ _ v _) = (v, toD p)
toDD (Silent d) = error "wtf"

-- Converts single note to temp
toTemp :: Pch -> Track Sig (D, D)
toTemp p@(Pch n o v d) = str d . temp . toDD $ p
toTemp (Silent d) = rest d

-- Converts notes to temps
toTemps :: [Pch] -> [Track Sig (D, D)]
toTemps = fmap toTemp

-- Converts notes to melody
toMel :: [Pch] -> Track Sig (D, D)
toMel = mel . toTemps

-- Converts notes to chord
toChord :: [Pch] -> Track Sig (D, D)
toChord = har . toTemps

-- Utility for easy conversion of notes to a standard velocity / duration
simpleNotes :: [Note] -> Octave -> Velocity -> Duration -> [Pch]
simpleNotes notes o v d = Pch <$> notes <*> pure o <*> pure v <*> pure d
