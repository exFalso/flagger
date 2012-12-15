{-# LANGUAGE GeneralizedNewtypeDeriving, NamedFieldPuns, DeriveDataTypeable #-}
module Flagger where

import Flag
import Stats
import Log
import F

import SocketProducer
import RawSocketProducer

import Network
import Control.Monad.Reader
import Control.Concurrent.STM
import System.IO
import qualified System.Timeout as T
import Text.Regex.TDFA.String
import Text.Regex.TDFA
import System.Exit
import System.Console.CmdArgs
import Data.Maybe

_MONITOR_PORT :: PortNumber
_MONITOR_PORT = 7776
_SOCKET_PORT :: PortNumber
_SOCKET_PORT = 7777
_RAW_SOCKET_PORT :: PortNumber
_RAW_SOCKET_PORT = 7778

producers :: [FlagProducer]
producers = [ socketProducer _SOCKET_PORT
            , rawSocketProducer _RAW_SOCKET_PORT]

flagger :: String -> ([Flag] -> IO (Maybe [Bool])) -> IO ()
flagger regexp submit = do
  let configCmd = Config { timeout = 10 &= help "submit timeout, seconds"
                         , interval = 5 &= help "wait interval, seconds"
                         , logFilename = "flagger.log" &= help "logfile"
                         , wflagFile = "flagger.wflag" &= help "wrong flags file"
                         , rflagFile = "flagger.rflag" &= help "right flags file"
                         , iflagFile = "flagger.iflag" &= help "invalid flags file"
                         } &= summary "Flagger"
  
  config <- cmdArgs configCmd

  tvarStats <- newTVarIO $ Stats 0 0 0
  tvarFlags <- newTVarIO []
  runF tvarStats "monitor" (logFilename config) $
       do
         logI "Starting ticker..."
         _ <- forkF "ticker" $ ticker submit config tvarFlags
         logI "Starting listener..."
         _ <- forkF "listener" $ listener regexp config tvarFlags producers
         monitor _MONITOR_PORT tvarStats tvarFlags

data Config
    = Config { timeout :: Int   -- submit timeout, seconds
             , interval :: Int  -- wait interval, seconds
             , logFilename :: String -- log
             , wflagFile :: String   -- wrong flags
             , rflagFile :: String   -- right flags
             , iflagFile :: String   -- invalid flags
             }
      deriving (Show, Data, Typeable)

partitionWith :: [Bool] -> [a] -> ([a], [a])
partitionWith as bs = foldl (\(l, r) (b, f) ->
                                 if b
                                 then (f : l, r)
                                 else (l, f : r)) ([], []) $ zip as bs

monitor :: PortNumber -> TVar Stats -> TVar [Flag] -> F ()
monitor p tvStats tvFlags = do
  logI $ "Monitoring on port " ++ show p
  s <- liftIO $ listenOn (PortNumber p)
  forever $ do
    (h, n, _) <- liftIO $ accept s
    logI $ "Connection from " ++ n
    stats <- liftIO . atomically $ readTVar tvStats
    flags <- liftIO . atomically $ readTVar tvFlags
    liftIO $ hPutStr h (show (length flags, stats)) >> hFlush h

ticker :: ([Flag] -> IO (Maybe [Bool])) -> Config -> TVar [Flag] -> F ()
ticker submit Config { interval, timeout, wflagFile, rflagFile } tvar
    = do
  wfh <- liftIO $ openFile wflagFile AppendMode
  rfh <- liftIO $ openFile rflagFile AppendMode
  forever $ do
    flags <- liftIO . atomically $ do
               fs <- readTVar tvar
               if null fs
               then retry
               else writeTVar tvar [] >> return fs
    bvar <- liftIO $ registerDelay interval
    let nflags = length flags
    logI $ "Submitting " ++ show nflags ++ " flags"
    mbs <- liftIO $ T.timeout timeout (submit flags)
    case mbs of
      Nothing -> do
               logW "Submission timed out, retrying..."
      Just Nothing -> return ()
      Just (Just bs) -> do
               if length bs /= nflags
               then logE $ "Submission function incorrect, returned list with " ++
                    show (length bs) ++ " elements instead of " ++ show nflags
               else do
                 let (af, naf) = partitionWith bs flags

                 when (naf /= []) $ do
                      logE $ show (length naf) ++ " flags not accepted"
                      liftIO . hPutStr wfh $ concatMap ((++ "\n") . show) af
                           
                 when (af /= []) $ do
                      logS $ show (length af) ++ " flags successfully submitted"
                      liftIO . hPutStr rfh $ concatMap ((++ "\n") . show) naf
                             
    liftIO . atomically $ readTVar bvar >>= \b -> if b then return () else retry

listener :: String -> Config -> TVar [Flag] -> [FlagProducer] -> F ()
listener flagRegexp Config { iflagFile } tv ps = do
  ifh <- liftIO $ openFile iflagFile AppendMode
  -- compile regexp
  reg <- case compile blankCompOpt blankExecOpt flagRegexp of
    Left err -> do
      logE $ "Invalid regular expression \"" ++ flagRegexp ++ "\": " ++ err
      liftIO $ exitFailure
    Right reg -> return reg
  -- start producers
  chan <- liftIO newTChanIO
  mapM_ (\(FlagProducer n f) -> forkF n $ f chan) ps
  -- main loop
  forever $ do
    SrcFlags src fs <- liftIO . atomically $ readTChan chan
    let bs = map (\(Flag f) -> either (const False) isJust . execute reg $ f) fs
        (cfs, ifs) = partitionWith bs fs
    logI $ "Received " ++ show (length bs) ++ " flags from '" ++ src ++ "'"
    when (ifs /= []) $ do
              logE $ show (length ifs) ++ " flags don't match the regexp \""
                       ++ flagRegexp ++ "\""
              liftIO . hPutStr ifh $ concatMap ((++ "\n") . show) ifs
    liftIO . atomically $ modifyTVar tv (cfs ++)
