module Person where

import Data.List
import Data.Time
import Data.Maybe

import Date

type Name = String
type FamilyName = String
type Company = String
type Telephone = String
type Mail = String
type Birthday = Maybe Date

-- Show i Read beda uzywane do zapisu do / odczytu z pliku - i niczego wiecej
-- Wyświetlanie będzie realizowała funkcja printablePerson
-- Unikalnym identyfikatorem osoby będzie jej adres email

data Person = Person {
  name :: Name, 
  familyName :: FamilyName, 
  company :: Company, 
  telephone :: Telephone, 
  mail :: Mail, 		--UNIKALNY IDENTYFIKATOR
  birthday :: Birthday }  deriving (Show, Read)
  
-- używana do sortowania
instance Ord Person where
  compare a b 
    | a == b				= EQ
    | familyName a /= familyName b 	= compare (familyName a) (familyName b)
    | name a /= name b			= compare (name a) (name b)
    | birthday a /= birthday b		= compare (birthday a) (birthday b)
    | mail a /= mail b			= compare (mail a) (mail b)
    | otherwise 			= EQ
    
instance Eq Person where
  (==) a b = mail a == mail b
    
    
-- funkcja konwertujaca "Person" na czytelny napis
printablePerson :: Person -> String
printablePerson p = concat $ intersperse " " [name p, familyName p, company p, telephone p, mail p, printableDate (birthday p)]


-- funkcja sprawdzajaca, czy dana osoba ma dzisiaj urodziny
-- konwersja wszystkich Int na Integer, w aplikacji uzywamy wylacznie Integerow
hasBirthday :: Date -> Person  -> Bool 
hasBirthday (Date date) person
  | isNothing $ birthday person 	= False
  | otherwise 				= isAnniversary (fromJust $ birthday person) day month where
  (_, m, d) = toGregorian date
  month = toInteger m
  day = toInteger d


  
  
-- hasBirthday :: Person -> IO Bool 
-- hasBirthday person = do
--   c <- getCurrentTime
--   let (_, m, d) = toGregorian $ utctDay c
--       month = toInteger m
--       day = toInteger d
--   return (isAnniversary (fromJust $ birthday person) day month)