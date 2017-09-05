#!/usr/bin/env runhaskell

module Main where

import System.Environment
import System.IO

main :: IO ()
main = do
  [amps, ns] <- getArgs
  let amp = sToInt amps
  let n = sToInt ns
  let table = sines sin (intToDouble amp) n
  --printBars table
  putStrLn $ show table

psines fun amp n =
  fmap (\x -> round ((amp * fun x) / 2 + half)) (periodRange n)
  where
    half = amp / 2

sines :: (Double -> Double) -> Double -> Int -> [Int]
sines fun amp n = fmap (\x -> round (amp * fun x)) (periodRange n)

sToInt :: String -> Int
sToInt = read

intToDouble :: Int -> Double
intToDouble i = fromIntegral i :: Double

intToRad :: Int -> Double
intToRad i = (intToDouble i) / 180 * pi

periodRange :: Int -> [Double]
periodRange n = fmap (\x -> (intToDouble x) * slice) [0..(n-1)]
  where
    slice = 2 * pi / (intToDouble n)

printBars is = mapM_ (\i -> putStrLn (take i (repeat '*'))) is
