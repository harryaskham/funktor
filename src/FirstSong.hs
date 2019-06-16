module FirstSong where

import Csound.Base hiding (Tab)
import Csound.Catalog.Drum.Tr808
import Csound.Patch
import Csound.Sam
import Csound.Sam.Core
import Data.List.Split
import Melodies
import Tabs

-- TODO: Once in a state to split up, do so into commented composable utils

-- Global controls + derivations
-- TODO: Might also need to pair this with 'setBpm x >> song'
bpm = 140
bps = 140 / 60
spb = 1 / bps

kicks = compileTab $ DrumTab "O___|.___|o___|.___" bd
hats = compileTab $ DrumTab "___ o|O___" chh
snares = compileTab $ DrumTab "O___|.___" sn

-- Takes a list of drum tracks and compiles to a signal.
compileDrums :: [Sample Sig2] -> SE Sig2
compileDrums = runSam (bpm * 4) . sum

drums = compileDrums [kicks, hats, snares]

-- Instrument defn
oscInstr :: D -> SE Sig
oscInstr x = return $ mul (linsegr [0, 0.03, 1, 0.2, 0] 0.1 0) $ osc $ sig x

-- Compiles the given track using the given instrument, ensuring BPM matches.
-- TODO: Extend to non-mono, non-simple instruments
compileTrack :: (D -> SE Sig) -> Track Sig D -> Sig
compileTrack instr = mix . sco instr . fmap cpspch . str spb

melody = fromMono . compileTrack oscInstr $ twinkle

song = sum [pure melody, drums]
