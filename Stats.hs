module Stats where

data Stats
    = Stats { submittedAccepted :: Int
            , submittedNotAccepted :: Int
            , invalid :: Int
            }
      deriving (Show, Read)