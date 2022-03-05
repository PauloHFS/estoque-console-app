module Lib
  ( menu,
  )
where

import Data.Time
import Storage
  ( Produto,
    createProduct,
    created_at,
    nome,
    preco,
    quantidade,
    readStorage,
    uid,
    updateUid,
    updated_at,
    validade,
    verifyStorage,
    verifyValidade,
    writeStorage,
  )

menu :: IO ()
menu = do
  produtos <- readStorage
  putStrLn ""
  putStrLn "Estoque Console App - Haskell Edition"
  putStrLn ""
  putStrLn "Comandos:"
  putStrLn "c       - Adiciona um novo produto ao inventário"
  putStrLn "l       - Lista todos os produtos do inventário"
  putStrLn "mq      - Modifica a quantidade de um produto"
  putStrLn "mp      - Modifica o preço de um produto"
  putStrLn "d       - Remove um produto do inventário"
  putStrLn "v       - Verifica validade dos produtos"
  putStrLn "z       - Verifica itens zerados"
  putStrLn "q       - Sair"
  prompt produtos

prompt :: [Produto] -> IO ()
prompt produtos = do
  putStrLn "> "
  command <- getLine
  interpret command produtos

interpret :: String -> [Produto] -> IO ()
interpret "c" produtos = create produtos
interpret "l" produtos = list produtos
interpret "mq" produtos = updateQuantity produtos
interpret "mp" produtos = updatePrice produtos
interpret "d" produtos = delete produtos
interpret "v" produtos = filterByValidade produtos
interpret "z" produtos = filterByQuantityZero produtos
interpret "q" produtos = return ()
interpret command produtos = do
  putStrLn ("Invalid command: `" ++ command ++ "`")
  prompt produtos

create :: [Produto] -> IO ()
create produtos = do
  let uid = length produtos
  putStrLn "Digite o nome do produto: "
  name <- getLine
  putStrLn "Digite a quantidade do produto: "
  quantidade <- getLine
  putStrLn "Digite o preco do produto: "
  preco <- getLine
  putStrLn "Digite a validade do produto (em meses): "
  validade <- getLine

  current <- getCurrentTime
  let today = formatDate current
  let product = createProduct uid name quantidade preco validade today today
  writeStorage (product : produtos)
  prompt (product : produtos)

list :: [Produto] -> IO ()
list produtos = do
  putStrLn ""
  putStrLn "uid | nome | quantidade | preco | validade | created_at | updated_at"
  mapM_ print produtos
  putStrLn ""
  prompt produtos

delete :: [Produto] -> IO ()
delete produtos = do
  putStrLn "Digite o uid do produto: "
  uid <- getLine
  let produtos' = filter (\p -> getUid p /= read uid) produtos

  --Update the Uid to avoid duplicates
  let produtos'' = updateUid produtos' (read uid)

  prompt produtos''

updateQuantity :: [Produto] -> IO ()
updateQuantity produtos = do
  putStrLn "Digite o uid do produto: "
  uid <- getLine
  putStrLn "Digite a nova quantidade: "
  newQuantity <- getLine

  -- get a product by uid
  let produto = head (filter (\p -> getUid p == read uid) produtos)
  let produtos' = filter (\p -> getUid p /= read uid) produtos

  let produto' = createProduct (read uid) (nome produto) newQuantity (preco produto) (validade produto) (created_at produto) (updated_at produto)

  prompt (produto' : produtos')

getUid :: Produto -> Int
getUid = uid

updatePrice :: [Produto] -> IO ()
updatePrice produtos = do
  putStrLn "Digite o uid do produto: "
  uid <- getLine
  putStrLn "Digite o novo preço: "
  newPrice <- getLine

  -- get a product by uid
  let produto = head (filter (\p -> getUid p == read uid) produtos)
  let produtos' = filter (\p -> getUid p /= read uid) produtos

  let produto' = createProduct (read uid) (nome produto) (quantidade produto) newPrice (validade produto) (created_at produto) (updated_at produto)

  prompt (produto' : produtos')

filterByQuantityZero :: [Produto] -> IO ()
filterByQuantityZero produtos = do
  putStrLn ""
  putStrLn "uid | nome | quantidade | preco | validade | created_at | updated_at"
  print (verifyStorage produtos)
  prompt produtos

-- Filtra o estoque por produtos vencidos
filterByValidade :: [Produto] -> IO ()
filterByValidade produtos = do
  c <- getCurrentTime
  putStrLn ""
  putStrLn "uid | nome | quantidade | preco | validade | created_at | updated_at"
  mapM_ print $ verifyValidade produtos (utctDay c)
  prompt produtos

formatDate :: UTCTime -> String
formatDate = formatTime defaultTimeLocale "%d/%m/%0Y"