{-# LANGUAGE OverloadedStrings #-}

module Storage (readStorage) where

import Control.Monad (unless)
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V
import System.Directory (doesFileExist)

data Product = Product
  { uid :: Int,
    nome :: String,
    quantidade :: Int,
    preco :: Int,
    validade :: Int,
    created_at :: String,
    updated_at :: String
  }
  deriving (Show, Eq)

instance FromNamedRecord Product where
  parseNamedRecord r = Product <$> r .: "uid" <*> r .: "nome" <*> r .: "quantidade" <*> r .: "preco" <*> r .: "validade" <*> r .: "created_at" <*> r .: "updated_at"

readStorage :: IO [Product]
readStorage = do
  let filename = "storage.csv"
  fileExist <- doesFileExist filename

  unless fileExist $ writeFile filename "uid,nome,quantidade,preco,validade,created_at,updated_at\n"

  csvData <- BL.readFile filename
  case decodeByName csvData of
    Left err -> putStrLn err >> return []
    Right (_, v) -> return $ V.toList v