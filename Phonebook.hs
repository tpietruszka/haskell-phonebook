module Phonebook where

import Data.List
import Data.Time 

import Person
import Date
import Generic



-- obiekt przechowujący informacje o osobach i grupach, w formie listy
-- listy osób i grup są cały czas posortowane 
-- Show i Read beda uzywane do zapisu do / odczytu z pliku - i niczego wiecej
data Phonebook = Phonebook [Person] [Group] deriving (Show, Read)

-- dodaje osobę do książki telefonicznej, zakłada poprawność dostarczonych danych
addPerson :: Phonebook -> Person -> Phonebook
addPerson (Phonebook pList gList) person = Phonebook (insert person pList) gList

-- usuwa osobę - podaną jako cały obiekt
removePerson :: Phonebook -> Person -> Phonebook 
removePerson (Phonebook pList gList) person = Phonebook (delete person pList) gList

-- zamienia jedną podaną osobę na drugą. 
-- Jeśli pierwsza nie istnieje - generuje błąd
editPerson :: Phonebook -> Person -> Person -> Phonebook 
editPerson book@(Phonebook pList gList) old new = Phonebook (replace pList old new) gList

-- zwraca listę osób wg zadanej wartosci i kryterium, 
-- zadana wartość może być przedrostkiem wartości z książki, np 
-- findPeopleBy familyName "Kowals" book3
findPeopleBy :: (Person -> String) -> String -> Phonebook -> [Person]
findPeopleBy f value (Phonebook pList _) = filter ((isPrefixOf value) . f) pList

-- zwraca listę osób które dzisiaj obchodzą urodziny, 
-- jest "impure" bo pobiera aktualną datę z systemu
findBirthdayPeople :: Phonebook -> IO [Person]
findBirthdayPeople (Phonebook pList _) = do
  c <- getCurrentTime
  let today = Date $ utctDay c
  return (filter (hasBirthday today) pList)
  
-- zwraca listę osób należących do danej grupy
findPeopleInGroup :: Phonebook -> Group -> [Person]
findPeopleInGroup (Phonebook pList gList) g = filter (\x -> g `elem` (groups x)) pList
-- findPeopleInGroup (Phonebook pList gList) g = filter ((elem g).groups) pList -- równoznaczne 
  
-- dodaje grupę do książki, jeśli grupa już istnieje - nic się nie dzieje
addGroup :: Phonebook -> Group -> Phonebook
addGroup book@(Phonebook pList gList) newGroup 
  | not $ newGroup `elem` gList = (Phonebook pList newGroupList)
  | otherwise 			 = book
  where
    newGroupList = insert newGroup gList
  
-- usuwa grupę z listy i z wszystkich osób. Jeśli grupa nie istnieje - nic się nie dzieje
deleteGroup :: Phonebook -> Group -> Phonebook
deleteGroup (Phonebook pList gList) g = (Phonebook newPersonList newGroupList) where
  newPersonList = map (leaveGroup g) pList
  newGroupList = delete g gList

-- zmienia nazwę grupy (również w opisach osób). Zmiana nazwy grupy na istniejącą oznacza ich scalenie
renameGroup :: Phonebook -> Group -> Group -> Phonebook
renameGroup book@(Phonebook pList gList) old new = (Phonebook newPersonList newGroupList) where
  newGroupList = if not $ new `elem` gList
		    then sort $ new : afterDeletion
		    else afterDeletion
  afterDeletion = delete old gList
  newPersonList =  map (changeGroup old new) pList 
  
-- scala grupę a i b, grupa wynikowa ma nazwę a
mergeGroups :: Phonebook -> Group -> Group -> Phonebook
mergeGroups book old new = renameGroup book old new

-- dodaje daną osobę do grupy 
addPersonToGroup :: Phonebook -> Person -> Group -> Phonebook
addPersonToGroup book@(Phonebook pList gList) p g = editPerson book p (joinGroup g p)

-- usuwa daną osobe do grupy 
removePersonFromGroup :: Phonebook -> Person -> Group -> Phonebook
removePersonFromGroup book@(Phonebook pList gList) p g = editPerson book p (leaveGroup g p) 






urodziny = stringToDate "16.05.2012"
kowalski = Person "Jan" "Kowalski" "McDonalds" "+48654654" "asd@example.com" urodziny ["Rodzinne"]
nowak  = Person "Karol" "Nowak" "Tesco" "12321232" "nowak@asd.pl" (stringToDate "11.02.1980") ["Rodzinne"]

book0 = addPerson (addPerson (Phonebook [] ["Rodzinne"]) kowalski) nowak
book2 = addGroup book0 "Osobiste"
book3 = addPersonToGroup book2 kowalski "Osobiste"


