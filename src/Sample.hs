{-# LANGUAGE FlexibleContexts #-}

module Sample where

import Csound.Base hiding (Duration)
import Csound.Sam
import Control.Monad.Reader
import Tools
import Control.Lens

loadWav :: (MonadReader SongEnv m, MonadSE m) => Sig -> Sig -> String -> m (Seg Sig2)
loadWav beatLength stretch path =
  forBeats
    beatLength
    (toSeg $ scaleWav 0 stretch 1 path)
