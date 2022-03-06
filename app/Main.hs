module Main where

import Lib (menu, prompt)
import Storage (readStorage)

main :: IO ()
main = do
  produtos <- readStorage
  menu
  putStr "Total de produtos carregados: "
  print (length produtos)
  prompt produtos
