# funktor

## Overview
An opinionated music construction playground build on top of the excellent `csound-expression` library.

### Quick Example
A quick and dirty example song highlighting:

- Stochastic note generation
- Drum tab DSL
- `Applicative` melody construction

Doesn't sound great, mostly here for didactic purposes!
The `src` directory contains other songs as modules.

[Link to a .wav compiled from the below.](/example.wav?raw=true) GitHub doesn't seem to want to stream this, so `Save As...` if you want to hear it.

```haskell
song :: SongM
song = do
  -- 4 on the floor bass drum, on for 16 beats, off for 16 beats
  kicks <- do
    d <- drums "X _ _ _|O _ _ _|O _ _ _|O _ _ _" Tr808.bd2
    env <- sqrEnvM 0 16
    return (d & withEnv env)

  -- Closed hats, always on
  cHats <- drums "X O o ." Tr808.chh

  -- Open hats, always on
  oHats <- drums "o _ o _|_ _ o _" Tr808.ohh

  -- Group the durms
  let drms = har [kicks, cHats, oHats]

  -- Bassline - 5 notes from two chords at 2/3 velocity every 4 beats
  -- Oscillates in and out on a sinewave every 16 beats
  bass <- do
    let notes = take 5 $ minorChord D ++ reverse (majorChord Bb)
    compileI (withDeepBass 1.0 fmBass1) $ Pch <$> notes ?? 6 ?? (2/3) ?? 4

  -- Take 16 random notes from D minor, over 3 octaves, with some notes silent.
  lead <- do
    notes <-
      sequence
      $ replicate 16 randomFrom
      ?? expandScale [7, 8, 9] (minorScale D) ++ replicate 10 (\_ _ -> Silent (1/2))
    compileI razorLead $ notes ?? (1/2) ?? (1/2)

  -- Add some reverb FX to the instrs
  let instrs = har [ rever2 0.5 <$> bass
                   , rever2 0.2 <$> lead
                   ]

  -- Play all the above at once
  return $ drms =:= instrs

-- Set BPM and track length
songEnv :: SongEnv
songEnv = SongEnv { _bpm=140
                  , _beatLength=64
                  }
```

To play the song in real-time e.g. in GHCi:
```haskell
dac =<< runSongM songEnv song
```

Or to compile to a WAV:
```haskell
runToDisk =<< runSongM songEnv song
```

### Design

Follows `mtl` style, where a song is represented by a `SongM` monad transformer stack combining various effects useful for music creation:

- `RandT` for stochastic music generation
- `IOT` for loading samples, other external files
- `ReaderT` pattern over a song environment governing BPM and duration
- `SE` as the base monad (this is the `csound-expression` side-effecting monad)

```haskell
data SongEnv = SongEnv { _bpm :: Bpm
                       , _beatLength :: Int
                       }
type SongT = ReaderT SongEnv (RandT StdGen (IOT SE))
type SongM = SongT (Seg Sig2)
```

## Requirements
- `stack`
- `csound 6.14`
