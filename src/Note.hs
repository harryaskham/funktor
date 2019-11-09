module Note where

import Csound.Base hiding (Duration)
import System.Random
import Data.List
import Data.Functor ((<&>))
import Tools

data Note = C | Cs | Db | D | Ds | Eb | E | F | Fs | Gb | G | Gs | Ab | A | As | Bb | B deriving (Eq, Ord, Bounded, Show)
type Octave = Int
type Duration = Sig
type Velocity = D
data Pch = Pch Note Octave Velocity Duration | Silent Duration

instance Enum Note where
  fromEnum C = 0
  fromEnum Cs = 1
  fromEnum Db = 1
  fromEnum D = 2
  fromEnum Ds = 3
  fromEnum Eb = 3
  fromEnum E = 4
  fromEnum F = 5
  fromEnum Fs = 6
  fromEnum Gb = 6
  fromEnum G = 7
  fromEnum Gs = 8
  fromEnum Ab = 8
  fromEnum A = 9
  fromEnum As = 10
  fromEnum Bb = 10
  fromEnum B = 11

  toEnum 0 = C
  toEnum 1 = Cs
  toEnum 2 = D
  toEnum 3 = Eb
  toEnum 4 = E
  toEnum 5 = F
  toEnum 6 = Fs
  toEnum 7 = G
  toEnum 8 = Ab
  toEnum 9 = A
  toEnum 10 = Bb
  toEnum 11 = B
  toEnum x = toEnum (x `mod` 12)

-- Converts Pch note to its corresponding D e.g. 8.00 for mid-C
toD :: Pch -> D
toD (Pch n o v d) = octaveN + noteN
  where
    octaveN = fromIntegral o
    noteN = 0.01 * fromIntegral (fromEnum n)
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

-- Get n random things from list of things.
rndFrom :: RandomGen g => g -> Int -> [a] -> [a]
rndFrom g n xs = genericIndex xs <$> indices
  where
    indices = take n $ randomRs (0, length xs - 1) g

-- Takes a list of note weightings and converts to a list of notes ready to be randomized. 
weightsToPchs :: [(Note, Int)] -> [Octave -> Velocity -> Duration -> Pch]
weightsToPchs weights = Pch <$> concat (uncurry (flip replicate) <$> weights)

-- The definition of a scale in terms of semitone offsets.
newtype ScaleDef = ScaleDef [Int]
type ChordDef = ScaleDef

-- An alias for a scale as a list of notes.
type Scale = [Note]

-- Compile a scale with a root note.
toScale :: ScaleDef -> Note -> Scale
toScale (ScaleDef offsets) root = toEnum <$> (offsets <&> (+fromEnum root))

majorScale = toScale $ ScaleDef [0, 2, 4, 5, 7, 9, 11]
minorScale = toScale $ ScaleDef [0, 2, 3, 5, 7, 8, 10]

majorChord = toScale $ ScaleDef [0, 4, 7]
major7Chord = toScale $ ScaleDef [0, 4, 7, 9]
minorChord = toScale $ ScaleDef [0, 3, 7]
minor7Chord = toScale $ ScaleDef [0, 3, 7, 8]

minorChords n = [ minorChord n
                , majorChord (doN 3 succC n)
                , minorChord (doN 5 succC n)
                , minorChord (doN 7 succC n)
                , majorChord (doN 8 succC n)
                , majorChord (doN 10 succC n) ]
