module Main where

import Lib (menu, prompt)
import Storage (readStorage)

main :: IO ()
main = do
  produtos <- readStorage
  menu
  prompt produtos
