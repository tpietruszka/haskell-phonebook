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
		saveNewBook $ addPerson book personToAdd 

-- pobiera z konsoli dane o nowej osobie 
getPersonData = do --TODO: zrobić kontrolę wprowadzanych słów, można się posłużuć funkcją prompt' np. prompt' "Adres email" (\ x -> ("@" `elem` x) && ("." `elem` x) && (isLetter (last x))
		name <- promptLine "Imię" 
	        familyName <- promptLine "Nazwisko"  
		company  <- promptLine "Firma" 
		telephone  <- promptLine "Nr telefonu"
		mail  <- promptLine "Adres email" 
		birthday  <- promptLine "Data urodzin(dd.mm.rrrr)" 
		return $ Person name familyName company telephone mail (stringToDate birthday) []


-- **** WYPISYWANIE GRUPY KONTAKTÓW ****
printGroup = do book <- getBook
		groupName <- promptLine "Podaj nazwę grupy" 
		showBook "Kontakty w tej grupie" $ (Phonebook (findPeopleInGroup book groupName) [])
		pressEnter 


-- **** WYPISYWANIE KONTAKTU****
printContactsFile = getBook >>= showBook "Wszytskie kontakty"  >> pressEnter 

-- **** WYSZYKIWANIE KONTAKTU****
find byWhat functionAtEnd= do book <- getBook
			      value <- promptLine "Podaj prefix"
			      showBook "WYNIKI" (Phonebook (findPeopleBy byWhat value book) []) 
			      functionAtEnd (Phonebook (findPeopleBy byWhat value book) []) --TODO: jak to poprawnie napisać

-- funkcja pomocnicza do mechaniznu wyszukanie+wyswietlenie+edycja - ta funkcja pozwala tylko na wyszukanie i wyswietlenie bo "gubi" nazwę funkcji do wyjonania póżniej
pressEnter':: Phonebook -> IO ()
pressEnter' whatever = promptLine "Wcisnij ENTER aby kontynuowac.." >> return ()

-- **** EDYCJA & USUWANIE KONTAKTU****

whoFromResultsToEdit matchingGuysBook = if (inputLength > 1) 
						then do nr <- prompt' "Podaj numer kontaktu do edycji"  (\c -> c >= 1 && c <= inputLength)
							return $ getPerson matchingGuysBook nr 
						else return $ getPerson matchingGuysBook 1
							where inputLength = length (getPList matchingGuysBook)

editOrRemoveP matchingGuysBook = do persona <- whoFromResultsToEdit matchingGuysBook --TODO: moze tutaj dodac opcje dodaj do grupy?
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
				group <- promptLine "Do jakiej grupy dodać kontakt"
				book <- getBook
				saveNewBook $ addPersonToGroup book persona group 
				pressEnter


removePerFromGr matchingGuysBook = do persona <- whoFromResultsToEdit matchingGuysBook
	   			      group <- promptLine "Z jakiej grupy usunąć"
				      book <- getBook
				      saveNewBook $ removePersonFromGroup book persona group 	
				      pressEnter
			

-- **** EDYCJA GRUP ****

removeGroup = do book <- getBook
		 group <- promptLine "Podaj nazwę grupy do usunięcia"
		 if group `elem` getGList book
			then do saveNewBook $ deleteGroup book group 
				putStrFlush "Grupa usunieta! \n" >>pressEnter
			else putStrFlush "Taka grupa nie isnieje \n" >> pressEnter


--TODO: czemu te nie działają? i może warto zrobić "anuluj"
groupChangeName = do book <- getBook
		     groupToChange <- prompt' "Podaj nazwę istniejącej grupy do zmiany" (\g -> g `elem` getGList book) 
		     newGroup <- prompt' "Podaj nową nazwę (inną niż istniejące)" (\g -> not (g `elem` getGList book))
		     saveNewBook $ renameGroup book groupToChange newGroup
		     pressEnter 

sumGroups =  do book <- getBook
		group1 <- prompt' "Podaj nazwę istniejącej grupy do której ma zostać włączona inna" (\g -> g `elem` getGList book) 
		group2 <- prompt' "Podaj nazwę drugiej istniejącej grupy do scalenia" (\g -> g `elem` getGList book)
		saveNewBook $ mergeGroups book group1 group2
		pressEnter 



-- **** SPRAWDZANIE URODZIN ****
whoseBirthday = do book <- getBook
		   listP <- findBirthdayPeople book
		   showBook "Dzisiaj urodzinki mają:" (Phonebook listP [])
		   pressEnter


