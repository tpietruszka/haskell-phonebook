module Generic where

import Data.List
import Data.Maybe

 -- usuwa wszystkie elementy listy
deleteAll :: (Eq a) => a -> [a] -> [a]
deleteAll d list = [x | x <- list, x /= d]

-- zamienia pierwsze wystąpienie obiektu danego jako "old" na obiekt "new" w danej liście
-- jeśli zmiana mogła zakłócic sortowanie - sortuje listę
replace :: (Eq a, Ord a) => [a] -> a -> a -> [a]
replace list old new 
  | old `elem` list  = if (compare old new) == EQ
			  then result
			  else sort result
  | otherwise = error "Próba wymiany nieistniejącego elementu listy"
  where
    result = beginning ++ new : ending
    beginning = take position list
    ending = drop (position + 1) list
    position = fromJust $ elemIndex old list 
  

