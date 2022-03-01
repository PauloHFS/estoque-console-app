{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Storage (Product, readStorage) where

import Control.Monad (unless)
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V
import Helpers (centsToBRL)
import System.Directory (doesFileExist)

data Product = Product
  { uid :: Int,
    nome :: String,
    quantidade :: Int,
    preco :: Float,
    validade :: Int,
    created_at :: String,
    updated_at :: String
  }
  deriving (Eq)

instance Show Product where
  show (Product uid nome quantidade preco validade created_at updated_at) =
    show uid ++ " | " ++ nome ++ " | " ++ show quantidade ++ " | "
      ++ "R$ "
      ++ centsToBRL preco
      ++ " | "
      ++ show validade
      ++ " | "
      ++ created_at
      ++ " | "
      ++ updated_at

instance FromNamedRecord Product where
  parseNamedRecord r = Product <$> r .: "uid" <*> r .: "nome" <*> r .: "quantidade" <*> r .: "preco" <*> r .: "validade" <*> r .: "created_at" <*> r .: "updated_at"

instance ToNamedRecord Product where
  toNamedRecord (Product uid nome quantidade preco validade created_at updated_at) =
    namedRecord
      [ "uid" .= uid,
        "nome" .= nome,
        "quantidade" .= quantidade,
        "preco" .= preco,
        "validade" .= validade,
        "created_at" .= created_at,
        "updated_at" .= updated_at
      ]

readStorage :: IO [Product]
readStorage = do
  let filename = "storage.csv"
  fileExist <- doesFileExist filename

  unless fileExist $ writeFile filename "uid,nome,quantidade,preco,validade,created_at,updated_at\n"

  csvData <- BL.readFile filename
  case decodeByName csvData of
    Left err -> putStrLn err >> return []
    Right (_, v) -> return $ V.toList v