module Phonebook where

import Data.List
import Data.Time 

import Person
import Date

-- Show i Read beda uzywane do zapisu do / odczytu z pliku - i niczego wiecej
data Phonebook = Phonebook [Person] | Empty deriving (Show, Read)

-- dodaje osobę do książki telefonicznej, zakłada poprawność dostarczonych danych
addPerson :: Phonebook -> Person -> Phonebook
addPerson Empty person = Phonebook [person]
addPerson (Phonebook pList) person = Phonebook $ sort (person : pList)

-- usuwa osobę - podaną jako cały obiekt
removePerson :: Phonebook -> Person -> Phonebook 
removePerson Empty _ = Empty
removePerson (Phonebook pList) person = Phonebook [x | x <- pList, x /= person]

-- zamienia jedną podaną osobę na drugą. 
-- Jeśli pierwsza nie istnieje - generuje błąd
editPerson :: Phonebook -> Person -> Person -> Phonebook 
editPerson Empty _ _ = error "Próba edycji pustej książki"
editPerson (Phonebook pList) old new
  | old `elem` pList	= addPerson tempBook new 
  | otherwise  		= error "Próba zmiany danych osoby, której nie ma w książce"
  where 
    tempBook = (removePerson book old)

-- zwraca listę osób wg zadanej wartosci i kryterium, np 
-- findPeopleBy name book "Jan"
findPeopleBy :: (Person -> String) -> Phonebook -> String -> [Person]
findPeopleBy _ Empty _ = []
findPeopleBy f (Phonebook pList) value = filter ((==value) . f) pList

findBirthdayPeople :: Phonebook -> IO [Person]
findBirthdayPeople (Phonebook pList) = do
  c <- getCurrentTime
  let today = Date $ utctDay c
  return (filter (hasBirthday today) pList)

urodziny = stringToDate "16.05.2012"
kowalski = Person "Jan" "Kowalski" "McDonalds" "+48654654" "asd@example.com" urodziny
nowak  = Person "Karol" "Nowak" "Tesco" "12321232" "nowak@asd.pl" (stringToDate "11.02.1980")

book = addPerson (addPerson Empty kowalski) nowak
