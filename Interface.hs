module Interface (addContact) where
import Phonebook
import Person
import Terminal
import Date


dataFile = "contacts"

-- Function to add contact
addContact = do name <- promptLine "Imię" -- sprawdzanie czy nie ma cyfr
                familyName <- promptLine "Nazwisko"  
		company  <- promptLine "Firma" 
		telephone  <- promptLine "Nr telefonu" -- zadbac o formatowanie
		mail  <- promptLine "Adres email" -- czy jest @ i co najmniej jedna kropka?
		birthday  <- promptLine "Data urodzin(dd.mm.rrrr)" --zadbac o  formatwanie
		groups <- promptLine "Grupy" --zadbac o  formatwanie
                appendFile dataFile $ (show $ Person name familyName company telephone mail (stringToDate birthday) [groups]) ++ "\n"  
  

--printBook = do content <- readFile "contacts"
	       
		
		    
		


{-
view :: [String] -> IO ()  
view [fileName] = do  
    contents <- readFile fileName  
    let todoTasks = lines contents  
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks  
    putStr $ unlines numberedTasks  
-}




