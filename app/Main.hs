module Main where

import Lib (prompt)

main :: IO ()
main = do
  putStrLn ""
  putStrLn "Estoque Console App - Haskell Edition"
  putStrLn "Commands:"
  putStrLn ":l          - List all products"
  putStrLn ":q          - Quit"
  prompt