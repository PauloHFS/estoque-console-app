module Lib
  ( menu,
    prompt,
  )
where

import Data.Time
import Storage
import System.IO

menu :: IO ()
menu = do
  putStrLn "Estoque Console App - Haskell Edition"

  putStrLn "Comandos:"
  putStrLn "c       - Adiciona um novo produto ao inventário"
  putStrLn "l       - Lista todos os produtos do inventário"
  putStrLn "mq      - Modifica a quantidade de um produto"
  putStrLn "mp      - Modifica o preço de um produto"
  putStrLn "d       - Remove um produto do inventário"
  putStrLn "v       - Verifica validade dos produtos"
  putStrLn "z       - Verifica itens zerados"
  putStrLn "h       - Mostrar comandos"
  putStrLn "q       - Sair"

prompt :: [Produto] -> IO ()
prompt produtos = do
  hSetBuffering stdout NoBuffering
  putStrLn ""
  putStr "> "
  command <- getLine
  putStrLn ""
  interpret command produtos

interpret :: String -> [Produto] -> IO ()
interpret "c" produtos = create produtos
interpret "l" produtos = list produtos
interpret "mq" produtos = updateQuantity produtos
interpret "mp" produtos = updatePrice produtos
interpret "d" produtos = delete produtos
interpret "v" produtos = filterByValidade produtos
interpret "z" produtos = filterByQuantityZero produtos
interpret "h" produtos = do
  menu
  prompt produtos
interpret "q" produtos = do
  writeStorage produtos
  return ()
interpret command produtos = do
  putStrLn ("Comando inválido: `" ++ command ++ "`")
  prompt produtos

create :: [Produto] -> IO ()
create produtos = do
  let uid = length produtos
  putStrLn "Digite o nome do produto: "
  name <- getLine
  putStrLn "Digite a quantidade do produto: "
  inputQuantidade <- getLine
  let quantidade = read inputQuantidade :: Int
  putStrLn "Digite o preço do produto: "
  inputPreco <- getLine 
  let preco = read inputPreco :: Double
  putStrLn "Digite a validade do produto (em meses): "
  validade <- getLine

  current <- getCurrentTime
  let today = formatDate current
  let product = Produto uid name quantidade preco validade today today
  writeStorage (produtos ++ [product])

  prompt (produtos ++ [product])

list :: [Produto] -> IO ()
list produtos = do
  if null produtos
    then putStrLn "Nenhum produto cadastrado"
    else do
      putStrLn "uid | nome | quantidade | preco | validade | created_at | updated_at"
      mapM_ print produtos

  prompt produtos

delete :: [Produto] -> IO ()
delete produtos = do
  putStrLn "Digite o uid do produto: "
  uid <- getLine
  let produtos' = filter (\p -> getUid p /= read uid) produtos

  --Update the Uid to avoid duplicates
  let produtos'' = updateUid produtos' (read uid)

  writeStorage produtos''

  prompt produtos''

updateQuantity :: [Produto] -> IO ()
updateQuantity produtos = do
  putStrLn "Digite o uid do produto: "
  uid' <- getLine
  putStrLn "Digite a nova quantidade: "
  inputNewQuantity <- getLine
  let newQuantity = read inputNewQuantity :: Int

  -- get a product by uid
  let produto = head (filter (\p -> getUid p == read uid') produtos)
  let produtosL = filter (\p -> uid p < read uid') produtos
  let produtosR = filter (\p -> uid p > read uid') produtos

  let produto' = Produto (read uid') (nome produto) newQuantity (preco produto) (validade produto) (created_at produto) (updated_at produto)

  writeStorage (produtosL ++ produto' : produtosR)

  prompt (produtosL ++ produto' : produtosR)

getUid :: Produto -> Int
getUid = uid

updatePrice :: [Produto] -> IO ()
updatePrice produtos = do
  putStrLn "Digite o uid do produto: "
  uid' <- getLine
  putStrLn "Digite o novo preço: "
  inputNewPrice <- getLine
  let newPrice = read inputNewPrice :: Double

  -- get a product by uid
  let produto = head (filter (\p -> getUid p == read uid') produtos)
  let produtosL = filter (\p -> uid p < read uid') produtos
  let produtosR = filter (\p -> uid p > read uid') produtos

  let produto' = Produto (read uid') (nome produto) (quantidade produto) newPrice (validade produto) (created_at produto) (updated_at produto)

  writeStorage (produtosL ++ produto' : produtosR)

  prompt (produtosL ++ produto' : produtosR)

filterByQuantityZero :: [Produto] -> IO ()
filterByQuantityZero produtos = do
  let produtos' = verifyStorage produtos
  if null produtos'
    then putStrLn "Nenhum produto com quantidade igual a 0"
    else do
      putStrLn "uid | nome | quantidade | preco | validade | created_at | updated_at"
      mapM_ print produtos'
  prompt produtos

-- Filtra o estoque por produtos vencidos
filterByValidade :: [Produto] -> IO ()
filterByValidade produtos = do
  c <- getCurrentTime
  let produtos' = verifyValidade produtos (utctDay c)
  if null produtos'
    then putStrLn "Nenhum produto vencido"
    else do
      putStrLn "uid | nome | quantidade | preco | validade | created_at | updated_at"
      mapM_ print produtos'

  prompt produtos

formatDate :: UTCTime -> String
formatDate = formatTime defaultTimeLocale "%d/%m/%0Y"