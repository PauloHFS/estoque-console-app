module Lib
  ( prompt,
    interpret,
  )
where

import Storage (readStorage)

prompt :: IO ()
prompt = do
  putStrLn ""
  command <- getLine
  interpret command

interpret :: String -> IO ()
-- TODO: Melhorar o print do storage
interpret ":l" = do
  storage <- readStorage
  putStrLn ""
  print storage
  prompt
interpret ":q" = return ()
interpret command = do
  putStrLn ("Invalid command: `" ++ command ++ "`")
  prompt