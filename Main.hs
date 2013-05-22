import System.IO
import System.Exit
import Control.Monad

import Phonebook
import Terminal
import Interface
import Person




-- **********  main menu  ***********
main =  forever $ showMenu "MENU GŁÓWNE" 
	       [("Wyszukiwanie kontaktów", searchSubmenu),
		("Edytor kontaktów", editionSubmenu),
                ("Edytor grup kontaktów", groupsSubmenu),
                ("Kto ma dzisiaj urodziny?", whoseBirthday),
                ("Wyjście", exitSuccess)] 



-- ******* searchSubmenu *******
searchSubmenu= showMenu "WYSZUKIWANIE KONTAKTÓW WEDŁUG:"
	       [("Imienia",  Interface.findAndShow name), -- te funkcje beda musiały zapytac o imie badz wypisac liste gdy wciesniemy enter
                ("Nazwiska", Interface.findAndShow familyName),
                ("Firmy", Interface.findAndShow company),
                ("Nr telefonu", Interface.findAndShow telephone),
 		("Adresu email", Interface.findAndShow mail),
     -- 	("Daty urodzin", Interface.findAndShow birthday), -- trzeba rozkminic jak to zrobic przy tych typach
     --         ("Grupy", Interface.findAndShow groups),
		("<- Powrót", main)] 

-- ******** editionSubmenu *********
editionSubmenu = showMenu "EDYCJA KONTAKTOW" 
	       [("Wypisz kontakty", Interface.printContactsFile),
		("Nowy kontakt", Interface.addContact),
                ("Modyfikacja lub usunięcie kontaktu", editSubSubmenu),
                ("<- Powrót", main)] 

-- ******** editSubSubmenu *********
editSubSubmenu = showMenu "Modyfikacja lub usunięcie kontaktu" 
	       [("Wybór z listy wszystkich kontaktów", Interface.printAndEdit),
		("Wybór przez wyszukiwanie", whoseBirthday), --Interface.findAndEdit),
                ("<- Powrót", main)] 

-- ********* groupsSubmenu **********
groupsSubmenu= showMenu "EDYCJA GRUP" 
	       [("Nowa grupa kontaktów", whoseBirthday),
                ("Modyfikacja grupy", whoseBirthday),
                ("Usunięcie grupy", whoseBirthday),
                ("Scalenie grup", whoseBirthday),
                ("<- Powrót", main)] 

-- ***** whoseBirthday option  ******
whoseBirthday= putStrLn "gówno! haha!"

