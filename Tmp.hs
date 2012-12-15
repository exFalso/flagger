module Main where

import Flagger
import Flag

submit :: [Flag] -> IO (Maybe [Bool])
submit = return . Just . map (const True)

main :: IO ()
main = flagger "[0-9A-Z]{32}" submit
