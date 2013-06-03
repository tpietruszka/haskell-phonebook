import System.IO
import System.Exit
import Control.Monad

import Phonebook
import Terminal
import Interface
import Person
import Date

-- **********  main menu  ***********
main =  forever $ showMenu "MENU GŁÓWNE" 
	       [("Wyświetlenie wszystkich kontaków", Interface.printContactsFile),
		("Wyświetlenie grupy kontaków",  Interface.printGroup),
		("Wyszukiwanie kontaktów wg danych", searchSubmenu Interface.pressEnter'), 
		("Dodaj kontakt", Interface.addContact), 
		("Edytor kontaktów", searchSubmenu Interface.editOrRemoveP),
                ("Edytor grup kontaktów", groupsSubmenu),
                ("Kto ma dzisiaj urodziny?", Interface.whoseBirthday),
                ("Wyjście", exitSuccess)] 

-- ******* searchSubmenu *******
searchSubmenu nextFunction = showMenu "WYSZUKIWANIE KONTAKTÓW WEDŁUG:"
	       [("Imienia",  Interface.find name nextFunction), 
                ("Nazwiska", Interface.find familyName nextFunction),
                ("Firmy", Interface.find company nextFunction),
                ("Nr telefonu", Interface.find telephone nextFunction),
 		("Adresu email", Interface.find mail nextFunction),
     	        ("Daty urodzin (d.m.rrrr)", Interface.find (printableDate.birthday) nextFunction),
		("<- Powrót", main)] 

-- ********* groupsSubmenu **********
groupsSubmenu= showMenu "EDYCJA GRUP" 
	       [("Dodaj grupę", Interface.newGroup),
		("Wyświetl istniejące grupy", Interface.printAllGroups),
		("Dodaj osobę do grupy", searchSubmenu Interface.addPerToGr),
		("Usuń osobę z grupy", searchSubmenu Interface.removePerFromGr),
                ("Zmień nazwę grupy", Interface.groupChangeName),
		("Scal dwie grupy", Interface.sumGroups),
                ("Usunięcie grupy", Interface.removeGroup),
                ("<- Powrót", main)] 
