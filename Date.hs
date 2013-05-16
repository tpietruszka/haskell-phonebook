module Date where

import System.Locale
import Data.Time
import Data.Time.Format
import Data.Time.Calendar
import Data.Maybe



data Date = Date Day deriving (Show, Read, Eq, Ord)
-- używany w całej aplikacji format tekstowej reprezentacji daty:
dateFormat = "%-d.%-m.%Y"

-- funkcja formatująca datę do wyświetlenia
printableDate :: Maybe Date -> String
printableDate Nothing = ""
printableDate (Just (Date x)) = formatTime defaultTimeLocale dateFormat x

-- funkcja zamieniająca napis w określonym wcześniej formacie na Maybe Date, 
-- w przypadku błędnego formatu - Nothing
stringToDate :: String -> Maybe Date
stringToDate x 
  | isJust res = Just $ Date $ fromJust res
  | otherwise = Nothing
  where
    res = parseTime defaultTimeLocale dateFormat x :: Maybe Day
    
-- funkcja sprawdzajaca, czy dla podanej daty, dany dzien i miesiac sa rocznica
-- konwersja wszystkich Int na Integer, w aplikacji uzywamy wylacznie Integerow

isAnniversary :: Date -> Integer -> Integer -> Bool
isAnniversary (Date date) day month = (dateDay == day) && (dateMonth == month) where
  (_, m, d) = toGregorian date
  dateMonth = toInteger m
  dateDay = toInteger d

    
    
    
-- wersja "WLASNA"


-- type Day = Int
-- type Month = Int
-- type Year = Int 
-- 
-- data Date = Date { day :: Day,  month :: Month,  year :: Year} deriving (Eq)
-- 
-- instance Show Date where
--   show (Date d m y) = show d ++ "." ++ show m ++ "." ++ show y
-- 
-- 
-- -- na podstawie 3 podanych liczb zwraca datę, lub Nothing, jesli bylaby niepoprawna
-- makeDate :: Day -> Month -> Year -> Maybe Date
-- makeDate d m y 
--   | d <= daysInMonth m y && m <= 12 && d > 0 && m > 0 = Just (Date d m y)
--   | otherwise = Nothing
--   
-- parseDateString :: String -> Maybe Date
-- parseDateString "" = Nothing
-- parseDateString s 
--   | length parsed == 3 = makeDate (parsed !! 0) (parsed !! 1) (parsed !! 2)
--   where parsed = map read $ parseDotSeparated s
-- 
-- 
--   
-- -- funkcja dzielaca napis - liczby rozdzielone kropkami -  na listę napisów
-- parseDotSeparated ::  String -> [String]
-- parseDotSeparated [] = [""]
-- parseDotSeparated (x:xs) 
--    | x == '.' = "" : rest
--    | otherwise = (x : head rest) : tail rest
--    where
--        rest = parseDotSeparated xs
-- 
-- -- funkcja zwraca ilosc dni w miesiacu (w okreslonym roku)
-- daysInMonth ::  Month -> Year -> Int
-- daysInMonth  2 year 
--       | isLeapYear year = 29  
--       | otherwise    = 28
-- daysInMonth month year 
--       | month `elem` [9,4,6,11] = 30
--       | otherwise = 31
-- 
-- -- funkcja sprawdzająca, czy rok jest przestępny
-- isLeapYear :: Year -> Bool
-- isLeapYear x = (x `mod` 4 == 0 && x `mod` 100 /= 0) || x `mod` 400 == 0
