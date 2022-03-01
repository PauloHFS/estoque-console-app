module Helpers (centsToBRL) where

centsToBRL :: Float -> String
centsToBRL cents = show (cents / 100)