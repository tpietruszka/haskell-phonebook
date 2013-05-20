--module DataStorage (loadItems, getItem, addItem, saveItems) where
module DataStorage (loadBook,overwriteBook) where

import Data.List
import System.IO
import System.Directory
import Terminal
import Phonebook

tempFile = "temp"


-- Function used to load all items from the specified file
loadBook :: [Char] -> IO Phonebook
loadBook fname =   do   System.Directory.createDirectoryIfMissing True "data/"
                        handleFile <- openFile (createFname fname) ReadWriteMode
                        idata <- loadData handleFile 
                        return idata

--odczytuje zawartość z pliku (ktory jest przekazny przez uchwyt)
loadData handleFile = do isEof <- hIsEOF handleFile
	                 if isEof then
	                     return (Phonebook [] [])
	                 else do
	                     contents <- hGetContents handleFile
	                     return (read (contents)::Phonebook)

--nadpisuje wskazany plik obiektem typu Phonebook
overwriteBook newbook dataFile = do
	writeFile tempFile' (show newbook)
	x <- doesFileExist dataFile'
	if x
	  then removeFile dataFile' >> renameFile tempFile' dataFile'
	  else renameFile tempFile' dataFile'
		where tempFile' = (createFname tempFile)
		      dataFile' = (createFname dataFile)
		      



{-
overwriteBook newbook dataFile = do
	(tempName, tempHandle) <- openFile (createFname tempName) ReadWriteMode -- tworzymy temp file
	hPrint tempHandle newbook
	hClose tempHandle 
	x <- doesFileExist (createFname fname)
	if x
	  then removeFile (createFname fname) >> renameFile tempName dataFile
	  else
	       renameFile tempName dataFile

-}



-- tworzy odpowiednia nazwe pliku
createFname fname = ("data/" ++ fname ++ ".data")

{-
-- Function used to get all data from the file                            
loadData handleFile = do isEof <- hIsEOF handleFile
	                 if isEof then
	                     return Nothing
	                 else do
	                     contents <- hGetContents handleFile
	                     return (Just (read contents))
	          
-}



{-      
-- Function used to save all items in the specified file
saveBook book fname = do  hFile <- openFile (createFname fname) WriteMode                        
                        hClose hFile
                        mapM_ ((flip addItem) fname) i

                  
-- Function used to add a new item to the specified file
addItem i fname =   do  appendFile (createFname fname) (show i)
                        appendFile (createFname fname) "\n"
                                             
-- Function used to get certain item in the specified file
getItem f fname =   do  items <- loadItems fname
                        return $ find f items

          


-}
