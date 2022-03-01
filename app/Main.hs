module Main where

import Lib (prompt)

main :: IO ()
main = do    
    putStrLn "Commands:"
    putStrLn "q          - Quit"
    prompt