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
	       [("Imienia",  Interface.find name), -- te funkcje beda musiały zapytac o imie badz wypisac liste gdy wciesniemy enter
                ("Nazwiska", Interface.find familyName),
                ("Firmy", Interface.find company),
                ("Nr telefonu", Interface.find telephone),
 		("Adresu email", Interface.find mail),
     -- 	("Daty urodzin", Interface.find birthday), -- trzeba rozkminic jak to zrobic przy tych typach
     --         ("Grupy", Interface.find groups),
		("<- Powrót", main)] 

-- ******** editionSubmenu *********
editionSubmenu = showMenu "EDYCJA KONTAKTOW" 
	       [("Wypisz kontakty", Interface.printContactsFile),--Interface.printContactsFile),
		("Nowy kontakt", Interface.addContact),
                ("Modyfikacja kontaktu", whoseBirthday),
                ("Usunięcie kontaktu", whoseBirthday),
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

