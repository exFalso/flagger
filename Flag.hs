{-# LANGUAGE DeriveGeneric, StandaloneDeriving #-}
module Flag where

import F

import Control.Concurrent.STM
import GHC.Generics
import Data.Aeson

newtype Flag
    = Flag String
      deriving (Eq, Ord, Generic)
instance FromJSON Flag where
instance ToJSON Flag where


data SrcFlags
    = SrcFlags { source :: String
               , flags :: [Flag]
               }
      deriving (Show, Generic)
instance FromJSON SrcFlags where
instance ToJSON SrcFlags where

instance Show Flag where
    show (Flag s) = s

data FlagProducer
    = FlagProducer { producerName :: String
                   , produce :: TChan SrcFlags -> F ()
                   }
