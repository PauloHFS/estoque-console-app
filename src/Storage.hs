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
    verifyValidade,
    uid,
    nome,
    quantidade,
    preco,
    validade,
    created_at,
    updated_at,
    updateUid,
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
import Data.Time (diffDays, getCurrentTime, defaultTimeLocale, UTCTime (utctDay), Day, formatTime, parseTimeM, addDays, addGregorianMonthsRollOver)
import Data.Maybe (fromJust)

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

{-
  Separate the list of produtos into 2
  Left - doenst need change in the Uid
  Right - need change in the Uid
-}
updateUid :: [Produto] -> Int -> IO ()
updateUid produtos oldUid = do
  let produtosL = filter (\p -> getUid p < oldUid) produtos
  let produtosR = filter (\p -> getUid p > oldUid) produtos 

  produtosL : updateUidAux produtosR
  
--Recursively changes the Uid of the products
updateUidAux :: [Produto] -> [Produto]
updateUidAux [] = []
updateUidAux produtos = 
  let produto = head produtos
  let produto' = createProduct ((read uid) - 1) (nome produto) (quantidade produto) (preco produto) (validade produto) (created_at produto) (updated_at produto) 
  
  produto' : updateUidAux (tail produtos)

verifyStorage :: [Produto] -> [Produto]
verifyStorage [] = []
verifyStorage produtos =
  if read (quantidade (head produtos)) <= 0
    then head produtos : verifyStorage (tail produtos)
    else verifyStorage (tail produtos)

-- Verifica a validade de um Produto em certa data. Retorna True se o produto estiver vencido
verifyValidadeProduto :: Produto -> Day -> Bool
verifyValidadeProduto produto dia = read (validade produto) /= 0 && diffDays dataValidade dia < 0
  where dataValidade = addGregorianMonthsRollOver (read $ validade produto) $ fromJust $ parseTimeM True defaultTimeLocale  "%d/%m/%0Y" (created_at produto)

-- Verifica a validade dos produtos em certa data. Retorna uma lista de produtos vencidos.
verifyValidade :: [Produto] -> Day -> [Produto]
verifyValidade [] dia = []
verifyValidade produtos dia =
  if verifyValidadeProduto (head produtos) dia
    then head produtos : verifyValidade (tail produtos) dia
    else verifyValidade (tail produtos) dia

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
