import System.IO
import System.Exit
import Control.Monad

import Phonebook
import Terminal
import Interface




-- **********  main menu  ***********
main =  forever $ showMenu "MENU GŁÓWNE" 
	       [("Wyszukiwanie kontaktów", searchSubmenu),
		("Edytor kontaktów", editionSubmenu),
                ("Edytor grup kontaktów", groupsSubmenu),
                ("Kto ma dzisiaj urodziny?", whoseBirthday),
                ("Wyjście", exitSuccess)] 



-- ******* searchSubmenu *******
searchSubmenu= showMenu "WYSZUKIWANIE KONTAKTÓW WEDŁUG:"
	       [("Imienia", whoseBirthday), -- te funkcje beda musiały zapytac o imie badz wypisac liste gdy wciesniemy enter
                ("Nazwiska", whoseBirthday),
                ("Firmy", whoseBirthday),
                ("Nr telefonu", whoseBirthday),
 		("Adresu email", whoseBirthday),
                ("Daty urodzin", whoseBirthday),
                ("Grupy", whoseBirthday),
		("<- Powrót", main)] 

-- ******** editionSubmenu *********
editionSubmenu = showMenu "EDYCJA KONTAKTOW" 
	       [("Wypisz kontakty", whoseBirthday),
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

