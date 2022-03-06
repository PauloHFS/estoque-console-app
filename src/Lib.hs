module Lib
  ( menu,
    prompt,
  )
where

import Data.Maybe (isJust)
import Data.Time
import Storage
import System.IO
import Validate

menu :: IO ()
menu = do
  putStrLn ""
  putStrLn "███████╗███╗   ███╗ █████╗ ██████╗ ████████╗    ███╗   ███╗ ██████╗ ███╗   ███╗████████╗"
  putStrLn "██╔════╝████╗ ████║██╔══██╗██╔══██╗╚══██╔══╝    ████╗ ████║██╔════╝ ████╗ ████║╚══██╔══╝"
  putStrLn "███████╗██╔████╔██║███████║██████╔╝   ██║       ██╔████╔██║██║  ███╗██╔████╔██║   ██║   "
  putStrLn "╚════██║██║╚██╔╝██║██╔══██║██╔══██╗   ██║       ██║╚██╔╝██║██║   ██║██║╚██╔╝██║   ██║   "
  putStrLn "███████║██║ ╚═╝ ██║██║  ██║██║  ██║   ██║       ██║ ╚═╝ ██║╚██████╔╝██║ ╚═╝ ██║   ██║   "
  putStrLn "╚══════╝╚═╝     ╚═╝╚═╝  ╚═╝╚═╝  ╚═╝   ╚═╝       ╚═╝     ╚═╝ ╚═════╝ ╚═╝     ╚═╝   ╚═╝   "
  putStrLn "\n"
  putStrLn "\t\t\tGERENCIAMENTO DE ESTOQUE RÁPIDO E SIMPLES"
  putStrLn "\n"
  putStrLn "Comandos:"
  putStrLn "c       - Adiciona um novo produto ao inventário"
  putStrLn "l       - Lista todos os produtos do inventário"
  putStrLn "mq      - Modifica a quantidade de um produto"
  putStrLn "mp      - Modifica o preço de um produto"
  putStrLn "d       - Remove um produto do inventário"
  putStrLn "v       - Verifica validade dos produtos"
  putStrLn "cv      - Verifica validade de um Produto"
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
interpret "cv" produtos = checaValidade produtos
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
  putStrLn "Digite o nome do produto: "
  name <- getLine
  putStrLn "Digite a quantidade do produto: "
  inputQuantidade <- getLine
  putStrLn "Digite o preço do produto: "
  inputPreco <- getLine
  putStrLn "Digite a validade do produto (no formato DD/MM/YYYY): "
  validade <- getLine
  if not (any isEmptyInput [name, inputQuantidade, inputPreco, validade])
    && all isNumber [inputQuantidade, inputPreco]
    && isValidQuantity (read inputQuantidade)
    && isValidPrice (read inputPreco)
    then do
      let uid = length produtos
      let quantidade = read inputQuantidade :: Int
      let preco = read inputPreco :: Double
      current <- getCurrentTime
      let isDateValid = maybe False (verifyVencido (utctDay current)) (parseDate validade)
      if isDateValid
        then do
          let product = Produto uid name quantidade preco validade
          writeStorage (produtos ++ [product])
          prompt (produtos ++ [product])
        else do
          putStrLn "Entrada Invalida: Data Invalida"
          prompt produtos
    else do
      putStrLn "Entrada invalida"
      prompt produtos

list :: [Produto] -> IO ()
list produtos = do
  if null produtos
    then putStrLn "Nenhum produto cadastrado"
    else do
      putStrLn "uid | nome | quantidade | preco | validade"
      mapM_ print produtos
  prompt produtos

delete :: [Produto] -> IO ()
delete produtos = do
  putStrLn "Digite o uid do produto: "
  uid' <- getLine
  if not (isEmptyInput uid') && isNumber (read uid') && isValidUid (length produtos) (read uid')
    then do
      let produtos' = filter (\p -> uid p /= read uid') produtos
      let produtos'' = updateUid produtos' (read uid') --atualiza o Uid dos produtos
      writeStorage produtos''
      prompt produtos''
    else do
      putStrLn "Entrada invalida"
      prompt produtos

{-
Verifica a validade de
-}
checaValidade :: [Produto] -> IO ()
checaValidade produtos = do
  putStrLn "Digite o uid do produto: "
  uid' <- getLine
  if not (isEmptyInput uid') && isNumber (read uid') && isValidUid (length produtos) (read uid')
    then do
      let produto = head (filter (\p -> uid p == read uid') produtos) -- get a product by uid
      c <- getCurrentTime
      let invalid = verifyValidadeProduto produto $ utctDay c
      if invalid
        then putStr "Fora da Validade"
        else putStr "Dentro da Validade"
    else do
      putStr "Entrada invalida"

  prompt produtos

updateQuantity :: [Produto] -> IO ()
updateQuantity produtos = do
  putStrLn "Digite o uid do produto: "
  uid' <- getLine
  putStrLn "Digite a nova quantidade: "
  inputNewQuantity <- getLine
  if not (any isEmptyInput [uid', inputNewQuantity])
    && all isNumber [uid', inputNewQuantity]
    && isValidUid (length produtos) (read uid')
    && isValidQuantity (read inputNewQuantity)
    then do
      let newQuantity = read inputNewQuantity :: Int
      let produto = head (filter (\p -> uid p == read uid') produtos)
      let produtosL = filter (\p -> uid p < read uid') produtos
      let produtosR = filter (\p -> uid p > read uid') produtos
      let produto' = Produto (read uid') (nome produto) newQuantity (preco produto) (validade produto)
      writeStorage (produtosL ++ produto' : produtosR)
      prompt (produtosL ++ produto' : produtosR)
    else do
      putStr "Entrada invalida"
      prompt produtos

updatePrice :: [Produto] -> IO ()
updatePrice produtos = do
  putStrLn "Digite o uid do produto: "
  uid' <- getLine
  putStrLn "Digite o novo preço: "
  inputNewPrice <- getLine
  if not (any isEmptyInput [uid', inputNewPrice])
    && all isNumber [uid', inputNewPrice]
    && isValidUid (length produtos) (read uid')
    && isValidPrice (read inputNewPrice)
    then do
      let newPrice = read inputNewPrice :: Double
      let produto = head (filter (\p -> uid p == read uid') produtos) -- get a product by uid
      let produtosL = filter (\p -> uid p < read uid') produtos
      let produtosR = filter (\p -> uid p > read uid') produtos
      let produto' = Produto (read uid') (nome produto) (quantidade produto) newPrice (validade produto)
      writeStorage (produtosL ++ produto' : produtosR)
      prompt (produtosL ++ produto' : produtosR)
    else do
      putStr "Entrada invalida"
      prompt produtos

filterByQuantityZero :: [Produto] -> IO ()
filterByQuantityZero produtos = do
  let produtos' = verifyStorage produtos
  if null produtos'
    then putStrLn "Nenhum produto com quantidade igual a 0"
    else do
      putStrLn "uid | nome | quantidade | preco | validade"
      mapM_ print produtos'
  prompt produtos

{-
Filtra o estoque por produtos vencidos
-}
filterByValidade :: [Produto] -> IO ()
filterByValidade produtos = do
  c <- getCurrentTime
  let produtos' = verifyValidadeEstoque produtos (utctDay c)
  if null produtos'
    then putStrLn "Nenhum produto vencido"
    else do
      putStrLn "uid | nome | quantidade | preco | validade "
      mapM_ print produtos'
  prompt produtos

-- TODO: Mover para Util
formatDate :: UTCTime -> String
formatDate = formatTime defaultTimeLocale "%d/%m/%0Y"

-- TODO: Mover para Util
verifyVencido :: Day -> Day -> Bool
verifyVencido currentDate userDate = diffDays userDate currentDate >= 0

-- TODO: Mover para Util
parseDate :: String -> Maybe Day
parseDate = parseTimeM True defaultTimeLocale "%d/%m/%0Y"