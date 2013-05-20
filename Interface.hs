module Interface (addContact) where
import Phonebook
import Person
import Terminal
import Date
import DataStorage

dataFile = "contacts"


---------------- dodawanie kontaktu
addContact :: IO ()
addContact = do name <- promptLine "Imię" -- sprawdzanie czy nie ma cyfr
                familyName <- promptLine "Nazwisko"  
		company  <- promptLine "Firma" 
		telephone  <- promptLine "Nr telefonu" -- zadbac o formatowanie
		mail  <- promptLine "Adres email" -- czy jest @ i co najmniej jedna kropka?
		birthday  <- promptLine "Data urodzin(dd.mm.rrrr)" --zadbac o  formatwanie
		groups <- promptLine "Grupa" --zadbac o  formatwanie, pytanie wielokrotne o kolejne grupy
                getBook >>= addAndSaveNew name familyName company telephone mail birthday groups

		--appendFile dataFile $ (show $ Person name familyName company telephone mail (stringToDate birthday) [groups]) ++ "\n"  
  		
getBook = DataStorage.loadBook dataFile  

addAndSaveNew name familyName company telephone mail birthday groups phonebook = 
  	   DataStorage.overwriteBook newbook dataFile
		where newbook = addPerson phonebook $ Person name familyName company telephone mail (stringToDate birthday) [groups]


--printBook = do content <- readFile "contacts"


{-	       
newPerson Person name familyName company telephone mail birthday groups =  do saveStation station 
                    where station = Station id name city
                          id = (getNextStationId stations) + 1

newStation name city stations =  do saveStation station 
                                    where station = Station id name city
                                          id = (getNextStationId stations) + 1
	    -}
{-

main = do        
    handle <- openFile "todo.txt" ReadMode  
    (tempName, tempHandle) <- openTempFile "." "temp"  
    contents <- hGetContents handle  
    let todoTasks = lines contents     
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks     
    putStrLn "These are your TO-DO items:"  
    putStr $ unlines numberedTasks  
    putStrLn "Which one do you want to delete?"     
    numberString <- getLine     
    let number = read numberString     
        newTodoItems = delete (todoTasks !! number) todoTasks     
    hPutStr tempHandle $ unlines newTodoItems  
    hClose handle  
    hClose tempHandle  
    removeFile "todo.txt"  
    renameFile tempName "todo.txt"
		
-}



{-
view :: [String] -> IO ()  
view [fileName] = do  
    contents <- readFile fileName  
    let todoTasks = lines contents  
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks  
    putStr $ unlines numberedTasks  
-}




