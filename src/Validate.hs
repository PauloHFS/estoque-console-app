module Validate where

import Data.Char (isDigit)
import Data.List (find)
import Storage (Produto)

isEmptyInput :: String -> Bool
isEmptyInput = (== "")

isValidQuantity :: Int -> Bool
isValidQuantity = (>= 0)

isValidPrice :: Int -> Bool
isValidPrice = (> 0)

isNumber :: String -> Bool
isNumber = all isDigit

isValidUid :: Int -> Int -> Bool
isValidUid tamanho uid = uid >= 0 && uid < tamanho

hasInvalidInputs :: [Bool] -> Bool
hasInvalidInputs = elem False