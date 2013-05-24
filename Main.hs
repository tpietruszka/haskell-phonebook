import System.IO
import System.Exit
import Control.Monad

import Phonebook
import Terminal
import Interface
import Person

 --TODO: 
 -- można (ale nie trzeba) zrobić jeszce jakieś eleganckie wyswietlenie całej książki, ale typu grupa1:\n \t kontakt1 \n kontakt2\n grupa2: \n \t kontakt3 \n kontakt4\n 
 -- można byłoby pobierać przy wywoływaniu z terminala  parametr - nazwę pliku na którym ma się cała zabawa odbywać - teraz na szwtywno jest  nazwa "contacts" do której dobraniana jest końcówka .data oraz folder /data
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
     	     -- ("Daty urodzin", Interface.find birthday nextFunction), -- TODO: cos tu nie smiga z typami
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




