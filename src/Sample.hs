{-# LANGUAGE FlexibleContexts #-}

module Sample where

import Csound.Base hiding (Duration)
import Csound.Sam
import Control.Monad.Reader
import Tools
import Control.Lens

loadSample :: (MonadReader SongEnv m, MonadSE m) => String -> m Sig2
loadSample path = do
  gBPM <- asks (view bpm)
  liftSE $ runSam gBPM (wav path)
