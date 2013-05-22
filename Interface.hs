module Interface (addContact, printContactsFile, findAndShow, printAndEdit) where
import Phonebook
import Person
import Terminal
import Date
import DataStorage
import Data.Char
dataFile = "contacts"


-- **** DODAWANIE KONTAKTU ****

--punkcje pomocnicze:
getBook = DataStorage.loadBook dataFile  
saveNewBook newBook = DataStorage.overwriteBook newBook dataFile

addContact = do book <- getBook
		personToAdd <- getPersonData
		saveNewBook (addPerson book personToAdd) 

getPersonData = do 
		name <- promptLine "Imię" 
	        familyName <- promptLine "Nazwisko"  --czy jakies sprawdzanie formatiowania? np. czy nie zawiera cyfr, no ale w sumie ktoś może chcieć sobie wpisywać np. Kasia2 :P
		company  <- promptLine "Firma" 
		telephone  <- promptLine "Nr telefonu" --czy jakies sprawdzanie formatiowania?
		mail  <- promptLine "Adres email" --prompt' "Adres email" (\ x -> ("@" `elem` x) && ("." `elem` x) && (isLetter (last x))
		birthday  <- promptLine "Data urodzin(dd.mm.rrrr)" --
		groups <- promptLine "Grupa" --zadbac o  formatwanie, pytanie wielokrotne o kolejne grupy
		return $ Person name familyName company telephone mail (stringToDate birthday) [groups]

-- **** WYPISYWANIE ****
printContactsFile = getBook >>= showBook "Wszytskie kontakty"  >> pressEnter 

-- **** WYSZYKIWANIE ****
findAndShow :: (Person -> String) -> IO ()
findAndShow byWhat = do book <- getBook
			value <- promptLine "Podaj prefix"
			showBook "WYNIKI" (Phonebook (findPeopleBy byWhat value book) []) >> pressEnter


-- **** EDYCJA/USUWANIE ****
printAndEdit = do book <- getBook
		  showBook "Wszytskie kontakty" book 
		  nr <- prompt' "Podaj numer kontaktu do edycji/usuniecia"  (\c -> c >= 1 && c <= length (getList book))
		  editOrDelete <- prompt' "Dostępne operacje:\n 1) edycja\n 2) usunięcie kontaktu \n Twój wybór"  (\c -> c `elem` [1,2])
		  if editOrDelete == 1
			then editContact (getPerson book nr)
			else deleteContact (getPerson book nr)
	
editContact oldPerson = do book <- getBook
			   putStrFlush "Podaj nowe dane kontaktu:\n"
			   newPerson <- getPersonData
			   saveNewBook $ editPerson book oldPerson newPerson
			   putStrFlush "Kontakt zmieniony\n" >> pressEnter


deleteContact personToDel = do book <- getBook
			       saveNewBook $ removePerson book personToDel 
			       putStrFlush "Kontakt usunięty\n" >> pressEnter


	       
--pomocnicza funkcja, zwraca osobe z listy ktora zostala wypisana jako ponumerowana
getPerson book num = (getList book) !! (num -1 )
--pomocnicza funkcja, zwraca tylko liste osob z Phonebook 
getList (Phonebook pList gList) = pList


{-
findAndReturnBook byWhat = do book <- getBook
			      value <- promptLine "Podaj prefix"
			      showBook "WYNIKI" (Phonebook (findPeopleBy byWhat value book) [])
			      return (Phonebook (findPeopleBy byWhat value book) [])  --TODO: to poprawic

findAndEdit = do  showAllOrFind <- prompt' "Wybierz: 1 - wyświetlenie wszytkich i wybór, 2 - wyszukanie kontaktu)"  (\c -> c `elem` [1,2])
		  tempBook <- getPossibleDesidions showAllOrFind
		  nr <- prompt' "Podaj numer kontaktu do edycji/usuniecia"  (\c -> c >= 1 && c <= length (getList tempBook))
		  editOrDelete <- prompt' "Wybierz: 1 - edycja, 2 - usunięcie kontaktu)"  (\c -> c `elem` [1,2])						  			
		  if editOrDelete == 1
			then editContact (getPerson tempBook nr)
			else deleteContact (getPerson tempBook nr)
				where getPossibleDesidions  showOrFind 
					| showOrFind == 1 =  printContactsFile >> getBook >>= return
					| otherwise = return findMenuWithReturnBook 

findMenuWithReturnBook = do showMenu "WYSZUKIWANIE KONTAKTÓW WEDŁUG:"
			       [("Imienia",  findMenuWithReturnBook name), -- te funkcje beda musiały zapytac o imie badz wypisac liste gdy wciesniemy enter
				("Nazwiska", findMenuWithReturnBook familyName),
				("Firmy", findMenuWithReturnBook company),
				("Nr telefonu", findMenuWithReturnBook telephone),
    			 -- 	("Daty urodzin", Interface.findAndShow birthday), -- trzeba rozkminic jak to zrobic przy tych typach
		     --         ("Grupy", Interface.findAndShow groups)],
		 		("Adresu email", findMenuWithReturnBook mail)] >>= return
			    


-}









