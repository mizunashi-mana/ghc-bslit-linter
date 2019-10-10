{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as Char8
import qualified System.IO as System

main :: IO ()
main = do
    putBSStrLn "ascii string"
    putBSStrLn "のっとばいとすとりんぐ"
  where
    putBSStrLn = Char8.hPutStrLn System.stdout
