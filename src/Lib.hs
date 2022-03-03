module Lib
  ( prompt,
    interpret,
  )
where

import qualified Data.Monoid as Storage
import Storage (Produto, createProduct, readStorage, storageToProducts, writeStorage)

prompt :: IO ()
prompt = do
  -- print "> " -- TODO: arrumar um jeito de mostrar isso aq
  command <- getLine
  interpret command

interpret :: String -> IO ()
-- TODO: Melhorar o print do storage
interpret "c" = create
interpret "l" = listStorage
interpret "q" = return ()
interpret command = do
  putStrLn ("Invalid command: `" ++ command ++ "`")
  prompt

create = do
  storage <- readStorage

  let uid = show (length storage + 1)
  putStrLn "Enter the name of the product: "
  name <- getLine
  putStrLn "Enter the quantity of the product: "
  quantidade <- getLine
  putStrLn "Enter the price of the product: "
  preco <- getLine
  putStrLn "Enter the validade of the product: "
  validade <- getLine

  let product = createProduct uid name quantidade preco validade ("00/00/0000" :: String) ("00/00/0000" :: String)
  let newStorage = product : storageToProducts storage
  writeStorage newStorage
  prompt

listStorage :: IO ()
listStorage = do
  storage <- readStorage
  putStrLn ""
  putStrLn "uid | nome | quantidade | preco | validade | created_at | updated_at"
  prettyPrint (storageToProducts storage)
  prompt

prettyPrint :: [Storage.Produto] -> IO ()
prettyPrint [] = return ()
prettyPrint product = do
  print (show (head product))
  prettyPrint (tail product)
