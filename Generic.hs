module Generic where

import Data.List
import Data.Maybe

 -- usuwa wszystkie elementy listy
deleteAll :: (Eq a) => a -> [a] -> [a]
deleteAll d list = [x | x <- list, x /= d]

-- zamienia pierwsze wystąpienie obiektu danego jako "old" na obiekt "new" w danej liście
replace :: (Eq a) => [a] -> a -> a -> [a]
replace list old new 
  | old `elem` list  = beginning ++ new : ending
  | otherwise = error "Próba wymiany nieistniejącego elementu listy"
  where
    beginning = take position list
    ending = drop (position + 1) list
    position = fromJust $ elemIndex old list 
  