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

song :: SongM
song = do
  s <- loadSample "samples/wiley816.wav"
  return $ toSeg s

songEnv = SongEnv { _bpm=12
                  , _beatLength=1024
                  }

sams' = runSongM songEnv song
sams = dac =<< sams'
