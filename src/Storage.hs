{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Storage
  ( Produto,
    readStorage,
    writeStorage,
    storageToProducts,
    createProduct,
    verifyStorage,
    uid,
    nome,
    quantidade,
    preco,
    validade,
    created_at,
    updated_at,
  )
where

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
  { uid :: Int,
    nome :: String,
    quantidade :: String,
    preco :: String,
    validade :: String,
    created_at :: String,
    updated_at :: String
  }
  deriving (Generic, Eq)

-- create a new instance of the Show class for Product
instance Show Produto where
  show p =
    show (uid p)
      <> " | "
      <> show (nome p)
      <> " | "
      <> show (quantidade p)
      <> " | "
      <> show (preco p)
      <> " | "
      <> show (validade p)
      <> " | "
      <> show (created_at p)
      <> " | "
      <> show (updated_at p)

instance FromRecord Produto

instance ToRecord Produto

-- read storage.csv file
readStorage :: IO (Either String [Produto])
readStorage = do
  let fileName = "storage.csv"
  exists <- doesFileExist fileName
  unless exists $ BL.writeFile fileName ""
  BL.readFile fileName Data.Functor.<&> (fmap (V.toList . fmap (\(uid, nome, quantidade, preco, validade, created_at, updated_at) -> Produto uid nome quantidade preco validade created_at updated_at)) . decode NoHeader)

verifyStorage :: [Produto] -> [Produto]
verifyStorage [] = []
verifyStorage produtos =
  if read (quantidade (head produtos)) <= 0
    then head produtos : verifyStorage (tail produtos)
    else verifyStorage (tail produtos)

-- verificaValidade :: Produto -> Bool
-- verificaValidade =

--verifyValidade :: [Produto] -> [Produto]
--verifyValidade [] = []
--verifyValidade produtos =

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

createProduct :: Int -> String -> String -> String -> String -> String -> String -> Produto
createProduct = Produto
