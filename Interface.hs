module Interface (addContact, printContactsFile, find) where
import Phonebook
import Person
import Terminal
import Date
import DataStorage
import Data.Char
dataFile = "contacts"


-- **** DODAWANIE KONTAKTU ****
addContact :: IO ()
addContact = do name <- promptLine "Imię" 
                familyName <- promptLine "Nazwisko"  --czy jakies sprawdzanie formatiowania? np. czy nie zawiera cyfr, no ale w sumie ktoś może chcieć sobie wpisywać np. Kasia2 :P
		company  <- promptLine "Firma" 
		telephone  <- promptLine "Nr telefonu" --czy jakies sprawdzanie formatiowania?
		mail  <- promptLine "Adres email" --prompt' "Adres email" (\ x -> ("@" `elem` x) && ("." `elem` x) && (isLetter (last x))
		birthday  <- promptLine "Data urodzin(dd.mm.rrrr)" --
		groups <- promptLine "Grupa" --zadbac o  formatwanie, pytanie wielokrotne o kolejne grupy
                getBook >>= addAndSaveNew name familyName company telephone mail birthday groups
  		
-- operacje na pliku
getBook = DataStorage.loadBook dataFile  

addAndSaveNew name familyName company telephone mail birthday groups phonebook = 
  	   DataStorage.overwriteBook newbook dataFile
	   where newbook = addPerson phonebook $ Person name familyName company telephone mail (stringToDate birthday) [groups]


-- **** WYPISYWANIE ****
printContactsFile = getBook >>= showBook "Kontakty"  >> pressEnter 


-- **** WYSZYKIWANIE ****
find :: (Person -> String) -> IO ()
find byWhat = do value <- promptLine "Podaj prefix"
		 book <- getBook   
		 showBook "WYNIKI" (Phonebook (findPeopleBy byWhat value book) [])
		 pressEnter



















