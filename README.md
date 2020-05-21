# funktor

## Requirements
- `stack`
- `csound 6.14`

## Overview
An opinionated music construction playground build on top of the excellent `csound-expression` library.

### Quick Example

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
