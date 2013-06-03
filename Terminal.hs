module Terminal where
import System.IO
import Control.Exception
import Phonebook
import Person


-- wypis i flush stdout
putStrFlush str =  do  putStr str
                       hFlush stdout

-- promptLine - zachęta i pobranie lini                       
promptLine what =   do  putStrFlush $ what ++ ": "
                        x <- System.IO.getLine
                        return x

-- prompt - zacheta i pobranie tekstu od uzytkownika, sprawdza poprawnosc i ewentualnie dopiero zwraca wynik
-- wejscie: tekst zachety, predykat sprawdzający poprawnosc wejscia
prompt' :: Read b => [Char] -> (b -> Bool) -> IO b
prompt' text f =    do  putStrFlush $ text ++ ": "
                        x <- try readLn :: (Read t0) => IO (Either SomeException t0) --Jerzy: ciekawe.
                        case x of -- Jerzy: hmm czy takie casy zwalniaja z uzycia catch?
                            Left e  ->  invalid
                            Right v ->  if (f v) then return v
                                        else invalid
  where invalid = putStrFlush "Błąd, spróbuj jeszcze raz: " >> prompt' text f
	
	
-- jak wyżej, ale operuje na strinach, nie wymaga określenia typu wartości zwracanej 
promptString' :: [Char] -> ([Char] -> Bool) -> IO [Char]
promptString' text f =    do  putStrFlush $ text ++ ": "
                              x <- getLine
                              if ( f x ) then return x
					 else  putStrFlush "Błąd, spróbuj jeszcze raz: " >> promptString' text f
-- formatowanie tutulu menu
createLabel label size fill =   let fillsize = (size - (length label) - 2)
                                    halffillL = fillsize `div` 2
                                    halffillR = halffillL + (fillsize `mod` 2)
                                in
                                    (replicate halffillL fill) ++ " " ++ label ++ " " ++ (replicate halffillR fill)   

-- Wyswietlanie listy bez naglowka                        
showItems title itemsList = showItemsAndComment title itemsList [] 

-- Wyswietlanie listy z komentarzem na poczatku
showItemsAndComment title itemsList  comment= 
    do  putStrLn $ "\n" ++ createLabel title 100 '-'
        if (comment /= []) then do
            putStrFlush $ " *) " ++ comment ++ "\n"
            putStrFlush $ menuText itemsList 1
        else
            putStrFlush $ menuText itemsList 1
        putStrLn $ createLabel "-" 100 '-'
        where
            menuText [] inum = []
            menuText (i:is) inum = " " ++ (show inum) ++ ") " ++ i ++ "\n" ++ menuText is (inum + 1)                                  

showBook title (Phonebook pList gList) = 
    do  putStrLn $ "\n" ++ createLabel title 100 '-'
        putStrFlush $ " *) " ++ comment ++ "\n"
        putStrFlush $ bookText pList 1
        putStrLn $ createLabel "-" 100 '-'
        where
            bookText [] _ = []
            bookText (i:is) inum = " " ++ (show inum) ++ ") " ++ (printablePerson i)++ "\n" ++ bookText is (inum + 1)                                  
	    comment = "Imię Nazwisko Firma Nr.telefonu Email Data_urodzin Grupa"

-- czekanie na Enter
pressEnter = promptLine "Wcisnij ENTER aby kontynuowac.." >> return ()

-- wyswietla menu z tytylem; wejscie: tytul, lista : (nazwa, funkcja)
showMenu :: [Char] -> [([Char], IO a)] -> IO a
showMenu title actionList = do  showItems title [fst i | i <- actionList]
                                choice <- prompt' "Twój wybór" (\c -> c >= 1 && c <= length actionList)
                                snd $ actionList !! (choice - 1)  




 
