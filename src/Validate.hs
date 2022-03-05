module Validate where

emptyInput :: String -> Bool
emptyInput input = input == "" 

negativeQuantity :: Int -> Bool
negativeQuantity quantity =  not (quantity < 0)

negativePrice :: Int -> Bool
negativePrice price = not (price <= 0)

notNumber :: String -> Bool
notNumber input = 
    result <- try (read input) :: (Either SomeException Int)
    case result of 
        Left False
        Right True

validUid :: [Produto] -> Int -> Bool
validUid produtos uid = uid > 0 && uid < length produtos

validInputs :: [Bool] -> Bool
validInputs inputs = (filter (\i -> i == False) input) == []