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

    
