{-# LANGUAGE DeriveGeneric #-}
module SocketProducer where

import F
import Flag
import Log

import Network
import Control.Monad
import Data.Aeson
import Control.Monad.IO.Class
import Control.Concurrent.STM
import qualified Data.ByteString.Lazy as BS

socketProducer :: PortNumber -> FlagProducer
socketProducer p = FlagProducer ("socket " ++ show p) (sProduce p)

sProduce :: PortNumber -> TChan SrcFlags -> F ()
sProduce p tchan = do
  logI $ "Starting socket producer on port " ++ show p
  s <- liftIO $ listenOn (PortNumber p)
  forever $ do
    (h, n, _) <- liftIO $ accept s
    logI $ "Connection from " ++ n
    c <- liftIO $ BS.hGetContents h
    case decode c of
      Nothing -> logE "Could not decode JSON"
      Just srcfs -> liftIO . atomically $ writeTChan tchan srcfs
    
