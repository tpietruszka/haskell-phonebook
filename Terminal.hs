module Terminal where
import System.IO
import Control.Exception


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
                    where invalid = putStrFlush "Invalid data. " >> prompt' text f


-- formatowanie tutulu menu
createLabel label size fill =   let fillsize = (size - (length label) - 2)
                                    halffillL = fillsize `div` 2
                                    halffillR = halffillL + (fillsize `mod` 2)
                                in
                                    (replicate halffillL fill) ++ " " ++ label ++ " " ++ (replicate halffillR fill)   
-- Wyswietlanie listy bez naglowka                        
showItems title itemsList = showItems' title itemsList False  

-- Wyswietlanie listy
-- wejscie: hasHeader - czy lista ma w pierwszym elemencie naglowek
showItems' title itemsList hasHeader = 
    do  putStrLn $ "\n" ++ createLabel title 100 '-'
        if (hasHeader) then do
            putStrFlush $ " *) " ++ (head itemsList) ++ "\n"
            putStrFlush $ menuText (tail itemsList) 1
        else
            putStrFlush $ menuText itemsList 1
        putStrLn $ createLabel "-" 100 '-'
        where
            menuText [] inum = []
            menuText (i:is) inum = " " ++ (show inum) ++ ") " ++ i ++ "\n" ++ menuText is (inum + 1)                                  

-- wyswietla menu z tytylem; wejscie: tytul, lista : (nazwa, funkcja)
showMenu :: [Char] -> [([Char], IO a)] -> IO a
showMenu title actionList = do  showItems title [fst i | i <- actionList]
                                choice <- prompt' "Your choice" (\c -> c >= 1 && c <= length actionList)
                                snd $ actionList !! (choice - 1)  




 
