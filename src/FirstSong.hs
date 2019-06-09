module FirstSong where

import Csound.Base
import Csound.Catalog.Drum.Tr808
import Csound.Sam

kicks = pat [3, 3, 2] bd
hats = pat [2, 1, 2, 1, 1, 4] chh
snares = pat [4] sn
