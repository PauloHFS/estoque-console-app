module Lib
  ( prompt,
    interpret,
  )
where

import Storage (Product, readStorage)

prompt :: IO ()
prompt = do
  -- putStr "> " -- TODO: arrumar um jeito de mostrar isso aq
  command <- getLine
  interpret command

interpret :: String -> IO ()
-- TODO: Melhorar o print do storage
interpret "l" = listStorage
interpret "q" = return ()
interpret command = do
  putStrLn ("Invalid command: `" ++ command ++ "`")
  prompt

listStorage :: IO ()
listStorage = do
  storage <- readStorage
  putStrLn ""
  putStrLn "uid | nome | quantidade | preco | validade | created_at | updated_at"
  prettyPrint storage
  prompt

prettyPrint :: [Product] -> IO ()
prettyPrint [] = return ()
prettyPrint product = do
  print (show (head product))
  prettyPrint (tail product)