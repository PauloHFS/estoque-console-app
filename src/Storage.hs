{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Storage (Produto, readStorage, writeStorage, storageToProducts, createProduct) where

import Control.Monad (unless)
import qualified Data.ByteString.Lazy as BL
import Data.Csv
  ( FromRecord,
    HasHeader (NoHeader),
    ToRecord,
    decode,
    encode,
  )
import qualified Data.Functor
import qualified Data.String as BL
import Data.Text (Text)
import Data.Vector (update)
import qualified Data.Vector as V
import GHC.Generics (Generic)
import qualified GHC.Read as BL
import System.Directory (doesFileExist)

data Produto = Produto
  { uid :: String,
    nome :: String,
    quantidade :: String,
    preco :: String,
    validade :: String,
    created_at :: String,
    updated_at :: String
  }
  deriving (Generic, Eq)

-- create a new instance of teh Show class for Product
instance Show Produto where
  show p =
    "Produto { uid = "
      <> show (uid p)
      <> ", nome = "
      <> show (nome p)
      <> ", quantidade = "
      <> show (quantidade p)
      <> ", preco = "
      <> show (preco p)
      <> ", validade = "
      <> show (validade p)
      <> ", created_at = "
      <> show (created_at p)
      <> ", updated_at = "
      <> show (updated_at p)
      <> "}"

instance FromRecord Produto

instance ToRecord Produto

-- read storage.csv file
readStorage :: IO (Either String [Produto])
readStorage = do
  let fileName = "storage.csv"
  exists <- doesFileExist fileName
  unless exists $ BL.writeFile fileName ""
  BL.readFile fileName Data.Functor.<&> (fmap (V.toList . fmap (\(uid, nome, quantidade, preco, validade, created_at, updated_at) -> Produto uid nome quantidade preco validade created_at updated_at)) . decode NoHeader)

writeStorage :: [Produto] -> IO ()
writeStorage products = do
  let fileName = "storage.csv"
  exists <- doesFileExist fileName
  unless exists $ BL.writeFile fileName ""
  BL.writeFile fileName $ encode products

storageToProducts :: Either String [Produto] -> [Produto]
storageToProducts storage = do
  let newStorage = case storage of
        Left err -> []
        Right products -> products
  newStorage

createProduct :: String -> String -> String -> String -> String -> String -> String -> Produto
createProduct = Produto