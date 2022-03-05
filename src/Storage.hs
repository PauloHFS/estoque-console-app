{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Storage where

import Control.Monad (unless)
import Data.List.Split
import Data.Maybe (fromJust)
import Data.Time (Day, UTCTime (utctDay), addDays, addGregorianMonthsRollOver, defaultTimeLocale, diffDays, formatTime, getCurrentTime, parseTimeM)
import GHC.Read (Read (readPrec))
import System.Directory
import System.IO

readStorage :: IO [Produto]
readStorage = do
  let filename = "storage.csv"

  exists <- doesFileExist filename
  unless exists $ writeFile filename ""

  storage <- openFile filename ReadMode
  conteudo <- hGetContents storage

  let linhas = lines conteudo
  let produtos = map convertToProduto linhas

  return produtos

writeStorage :: [Produto] -> IO ()
writeStorage produtos = do
  let filename = "storage.csv"

  exists <- doesFileExist filename
  unless exists $ writeFile filename ""

  storage <- openFile filename WriteMode

  let linhas = map convertToString produtos
  let conteudo = unlines linhas

  hPutStr storage conteudo
  hFlush storage

data Produto = Produto
  { uid :: Int,
    nome :: String,
    quantidade :: Int,
    preco :: Double,
    validade :: String,
    created_at :: String,
    updated_at :: String
  }
  deriving (Eq)

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

{-
  Separate the list of produtos into 2
  Left - doenst need change in the Uid
  Right - need change in the Uid
-}
updateUid :: [Produto] -> Int -> [Produto]
updateUid [] oldUid = []
updateUid produtos oldUid = do
  let produtosL = filter (\p -> uid p < oldUid) produtos
  let produtosR = filter (\p -> uid p > oldUid) produtos

  produtosL ++ updateUidAux produtosR

--Recursively changes the Uid of the products
updateUidAux :: [Produto] -> [Produto]
updateUidAux [] = []
updateUidAux produtos = do
  let produto = head produtos
  let produto' = Produto (uid produto - 1) (nome produto) (quantidade produto) (preco produto) (validade produto) (created_at produto) (updated_at produto)

  produto' : updateUidAux (tail produtos)

{-
Verifica os produtos que se esgotaram do estoque
-}
verifyStorage :: [Produto] -> [Produto]
verifyStorage [] = []
verifyStorage produtos =
  if quantidade (head produtos) <= 0
    then head produtos : verifyStorage (tail produtos)
    else verifyStorage (tail produtos)

{-
Verifica a validade de um Produto em certa data. 
Retorna True se o produto estiver vencido
-}
verifyValidadeProduto :: Produto -> Day -> Bool
verifyValidadeProduto produto dia = diffDays dataValidade dia <= 0
  where
    dataValidade = fromJust $ parseTimeM True defaultTimeLocale "%d/%m/%0Y" (validade produto)

{- 
Verifica a validade dos produtos em certa data. 
Retorna uma lista de produtos vencidos.
-}
verifyValidadeEstoque :: [Produto] -> Day -> [Produto]
verifyValidadeEstoque [] dia = []
verifyValidadeEstoque produtos dia =
  if verifyValidadeProduto (head produtos) dia
    then head produtos : verifyValidadeEstoque (tail produtos) dia
    else verifyValidadeEstoque (tail produtos) dia

convertToProduto :: String -> Produto
convertToProduto linha =
  let [uid, nome, quantidade, preco, validade, created_at, updated_at] = splitOn "," linha
   in Produto
        { uid = read uid,
          nome = read nome,
          quantidade = read quantidade,
          preco = read preco,
          validade = read validade,
          created_at = read created_at,
          updated_at = read updated_at
        }

convertToString :: Produto -> String
convertToString p =
  show (uid p)
    <> ","
    <> show (nome p)
    <> ","
    <> show (quantidade p)
    <> ","
    <> show (preco p)
    <> ","
    <> show (validade p)
    <> ","
    <> show (created_at p)
    <> ","
    <> show (updated_at p)
