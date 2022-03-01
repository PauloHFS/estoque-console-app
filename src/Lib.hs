module Lib
    ( prompt,
    interpret
    ) where

prompt :: IO ()
prompt = do
  putStrLn ""
  command <- getLine
  interpret command 

interpret :: String -> IO ()
interpret "q"  = return ()
interpret command  = do
  putStrLn ("Invalid command: `" ++ command ++ "`")
  prompt 