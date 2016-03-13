module Main where

import System.Console.Haskline(haskline)

main :: IO ()
main = do
    haskline "> " []
    return ()
