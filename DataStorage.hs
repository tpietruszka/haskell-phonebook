--module DataStorage (loadItems, getItem, addItem, saveItems) where
module DataStorage (loadBook,overwriteBook) where

import Data.List
import System.IO
import System.Directory
import Terminal
import Phonebook




-- Function used to load all items from the specified file
loadBook :: Read a => [Char] -> IO a
loadBook fname =   do   System.Directory.createDirectoryIfMissing True "data/"
			
                        handleFile <- openFile (createFname fname) ReadWriteMode
			putStrFlush "hi!"
                        idata <- loadData handleFile 
                        hClose handleFile
                        return idata


loadData handleFile = do contents <- hGetContents handleFile
			 return (read (contents))
-- co stanie sie jak plik bedzi pusty?


overwriteBook newbook dataFile = do
	(tempName, tempHandle) <- openTempFile "." "temp"
	hPrint tempHandle newbook
	hClose tempHandle 
	removeFile dataFile  
    	renameFile tempName dataFile 




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
