{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8      as Char8
import qualified Data.ByteString.Lazy.Char8 as LazyChar8
import qualified Data.Text.IO               as Text
import qualified System.IO                  as System

main :: IO ()
main = do
    putBSStrLn "ascii string"
    putBSStrLn "のっとばいとすとりんぐ"
    putStrLn "すとりんぐ"
    Text.putStrLn "すとりんぐ"
    putLBSStrLn "のっとれいじーばいとすとりんぐ"
  where
    putBSStrLn = Char8.hPutStrLn System.stdout
    putLBSStrLn = LazyChar8.hPutStrLn System.stdout
