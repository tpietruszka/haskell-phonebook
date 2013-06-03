module Interface (addContact,
		  printContactsFile, 
		  printGroup,
		  find, 
		  pressEnter', 
		  editOrRemoveP,
		  newGroup, 
		  printAllGroups,
		  addPerToGr,
		  removePerFromGr,
		  groupChangeName,
		  sumGroups,
		  removeGroup,
		  whoseBirthday) where


import Phonebook
import Person
import Terminal
import Date
import DataStorage
import Data.Char
import Data.Maybe


dataFile = "contacts"

-- **** FUNKCJE POMOCNICZE ****

--wczytuje książkę kontaktów z pliku dataFile
getBook = DataStorage.loadBook dataFile  
--nadpisuje globalny plik dataFile podaną książką kontatów
saveNewBook newBook = DataStorage.overwriteBook newBook dataFile
--zwraca osobe z listy kontaktów ktora zostala wypisana z numeracją dopisaną "w biegu"
getPerson book num = (getPList book) !! (num -1 )
--zwraca tylko listę osob z Phonebook 
getPList (Phonebook pList gList) = pList
--zwraca tylko listę grup z Phonebook 
getGList:: Phonebook -> [Group]
getGList (Phonebook pList gList) = gList

-- **** DODAWANIE KONTAKTU ****

-- ładuje książkę z pamięci, woła funkcję pobierajcą dane osoby, zapisuje książkę powiększoną o nowy wpis
addContact :: IO ()
addContact = do book <- getBook
		personToAdd <- getPersonData 
		if not (personToAdd `elem` (getPList book)) 
		  then saveNewBook $ addPerson book personToAdd 
		else putStrFlush "Taka osoba już jest w książce, kontakt nie został dodany"

-- pobiera z konsoli dane o nowej osobie 
getPersonData = do 
		name <- promptString' "Imię" validName
	        familyName <- promptString' "Nazwisko" validName
		company  <- promptLine "Firma" 
		telephone  <- promptString' "Nr telefonu" validPhone
		mail  <- promptString' "Adres email" validMail
		birthday  <- promptString' "Data urodzin(dd.mm.rrrr)" validDate
		return $ Person name familyName company telephone mail (stringToDate birthday) []
		
-- poprawne imiona i nazwiska mogą składać się z liter i spacjii, nie mogą być puste
validName :: String -> Bool 
validName x = (x /= "") && ( and $ map (\c -> isLetter c || c == ' ') x)

-- numer telefonu może składac się tylko z cyfr
validPhone :: String -> Bool 
validPhone x = and $ map isDigit x

--poprawny mail musi zawierać znaki '@' oraz '.' (-> nie może być pusty)
validMail :: String -> Bool
validMail x = '@' `elem` x && '.' `elem` x

-- poprawna data może być pusta, jeśli jest niepusta - musi być parsowalna
validDate :: String -> Bool 
validDate x = x == "" || (isJust $ stringToDate x)

-- **** WYPISYWANIE GRUPY KONTAKTÓW ****
printGroup = do book <- getBook
		groupName <- promptLine "Podaj nazwę grupy" 
		showBook "Kontakty w tej grupie" $ (Phonebook (findPeopleInGroup book groupName) [])
		pressEnter 

-- **** WYPISYWANIE KONTAKTU****
printContactsFile = getBook >>= showBook "Wszystkie kontakty"  >> pressEnter 

-- **** WYSZYKIWANIE KONTAKTU****
find byWhat functionAtEnd= do book <- getBook
			      value <- promptLine "Podaj prefix"
			      showBook "WYNIKI" (Phonebook (findPeopleBy byWhat value book) []) 
			      functionAtEnd (Phonebook (findPeopleBy byWhat value book) []) 

-- funkcja pomocnicza do mechaniznu wyszukanie+wyswietlenie+edycja - ta funkcja pozwala tylko na wyszukanie i wyswietlenie bo "gubi" nazwę funkcji do wyjonania póżniej
pressEnter':: Phonebook -> IO ()
pressEnter' whatever = promptLine "Wcisnij ENTER aby kontynuowac.." >> return ()

-- **** EDYCJA & USUWANIE KONTAKTU****

whoFromResultsToEdit matchingGuysBook = if (inputLength > 1) 
						then do nr <- prompt' "Podaj numer kontaktu do edycji"  (\c -> c >= 1 && c <= inputLength)
							return $ getPerson matchingGuysBook nr 
						else return $ getPerson matchingGuysBook 1
							where inputLength = length (getPList matchingGuysBook)

editOrRemoveP matchingGuysBook = do persona <- whoFromResultsToEdit matchingGuysBook 
				    editOrDelete <- prompt' "Dostępne operacje:\n 1) Edycja\n 2) Usunięcie kontaktu \n 3) Anuluj \n Twój wybór"  (\c -> c `elem` [1,2,3])
				    resultAction editOrDelete persona
					where resultAction x persona = case x of
								    1 -> editContact persona
								    2 -> deleteContact persona
								    3 -> return ()
editContact :: Person -> IO ()
editContact oldPerson = do book <- getBook
			   putStrFlush "Podaj nowe dane kontaktu:\n"
			   newPerson <- getPersonData
			   saveNewBook $ editPerson book oldPerson newPerson
			   putStrFlush "\t\t\t ---------------> Kontakt został zmieniony!\n" >> pressEnter

deleteContact :: Person -> IO ()
deleteContact personToDel = do book <- getBook
			       saveNewBook $ removePerson book personToDel 
			       putStrFlush "\t\t\t ---------------> Kontakt został usunięty!\n" >> pressEnter

-- **** DODAWANIE GRUPY ****

newGroup = do book <- getBook
	      group <- promptLine "Podaj nazwę grupy"
	      saveNewBook $ addGroup book group 

-- **** WYPISYWANIE ISTNIEJĄCYCH GRUP****

printAllGroups = do book <- getBook
		    showItems "Istniejące grupy" $ getGList book 
		    pressEnter 

-- **** DODAWANIE / USUWANIE KNTAKTÓW Z GRUP****

addPerToGr matchingGuysBook= do persona <- whoFromResultsToEdit matchingGuysBook	
				book <- getBook
				group <- promptString' "Do jakiej grupy dodać kontakt" (\x -> x `elem` (getGList book))
				saveNewBook $ addPersonToGroup book persona group 
				pressEnter

removePerFromGr matchingGuysBook = do persona <- whoFromResultsToEdit matchingGuysBook
				      book <- getBook
	   			      group <- promptLine "Z jakiej grupy usunąć"
				      saveNewBook $ removePersonFromGroup book persona group 	
				      pressEnter
			

-- **** EDYCJA GRUP ****

removeGroup = do book <- getBook
		 group <- promptLine "Podaj nazwę grupy do usunięcia"
		 if group `elem` getGList book
			then do saveNewBook $ deleteGroup book group 
				putStrFlush "Grupa usunieta! \n" >>pressEnter
			else putStrFlush "Taka grupa nie isnieje \n" >> pressEnter

groupChangeName = do book <- getBook
		     groupToChange <- promptString' "Podaj nazwę istniejącej grupy do zmiany" (\g -> g `elem` getGList book) 
		     newGroup <- promptString' "Podaj nową nazwę (inną niż istniejące)" (\g -> not (g `elem` getGList book))
		     saveNewBook $ renameGroup book groupToChange newGroup
		     pressEnter 

sumGroups =  do book <- getBook
		group1 <- promptString' "Podaj nazwę istniejącej grupy która ma zostać włączona do innej" (\g -> g `elem` getGList book) 
		group2 <- promptString' "Podaj nazwę drugiej istniejącej grupy do scalenia" (\g -> g `elem` getGList book)
		saveNewBook $ mergeGroups book group1 group2
		pressEnter 

-- **** SPRAWDZANIE URODZIN ****

whoseBirthday = do book <- getBook
		   listP <- findBirthdayPeople book
		   showBook "Dzisiaj urodzinki mają:" (Phonebook listP [])
		   pressEnter


