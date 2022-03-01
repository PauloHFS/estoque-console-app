module Main where

import Lib (prompt)

main :: IO ()
main = do
  putStrLn ""
  putStrLn "Estoque Console App - Haskell Edition"
  putStrLn "Commandos:"
  putStrLn "c <nome> <quantidade> <preco> <validade>  - Add a new product to the inventory"
  putStrLn "l                                         - Listar todos os produtos"
  putStrLn "q                                         - Sair"
  prompt