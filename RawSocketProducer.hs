{-# LANGUAGE DeriveGeneric #-}
module RawSocketProducer where

import F
import Flag
import Log

import Network
import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent.STM
import qualified Data.ByteString.Lazy.Char8 as BS

rawSocketProducer :: PortNumber -> FlagProducer
rawSocketProducer p = FlagProducer ("rsocket " ++ show p) (sProduce p)

sProduce :: PortNumber -> TChan SrcFlags -> F ()
sProduce p tchan = do
  logI $ "Starting raw socket producer on port " ++ show p
  s <- liftIO $ listenOn (PortNumber p)
  forever $ do
    (h, n, _) <- liftIO $ accept s
    logI $ "Connection from " ++ n
    c <- liftIO $ BS.hGetContents h
    liftIO . atomically . writeTChan tchan . SrcFlags "raw" . map fromBS $ BS.lines c


fromBS :: BS.ByteString -> Flag
fromBS = Flag . BS.unpack