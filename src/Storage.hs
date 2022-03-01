{-# LANGUAGE OverloadedStrings #-}

module Storage (readStorage) where

import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V

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

-- TODO: criar arquivo csv quando este não existir
readStorage :: IO [Product]
readStorage = do
  csvData <- BL.readFile "storage.csv"
  case decodeByName csvData of
    Left err -> putStrLn err >> return []
    Right (_, v) -> return $ V.toList v
