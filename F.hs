{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module F where

import Log
import Stats

import Control.Monad.Reader
import Control.Concurrent.STM
import Control.Concurrent

newtype F a
    = F { unF :: ReaderT (TVar Stats) Log a
        }
      deriving (Functor, Monad, MonadIO, MonadLog)

runF :: TVar Stats -> String -> String -> F a -> IO a
runF tvar tname fn f = runLogT tname fn $ runReaderT (unF f) tvar

forkF :: String -> F () -> F ThreadId
forkF tname f = do
  tvar <- F ask
  F . lift $ forkLog tname (runReaderT (unF f) tvar)
