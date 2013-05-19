module DataStorage (loadItems, getItem, addItem, saveItems) where

import Data.List
import System.IO
import System.Directory
import Terminal



-- Function used to load all items from the specified file
loadBook :: Read a => [Char] -> IO a
loadBook fname =   do  System.Directory.createDirectoryIfMissing True "data/"
                        handleFile <- openFile (createFname fname) ReadWriteMode
                        idata <- loadData handleFile
                        hClose handleFile
                        return idata


-- TUTAJ WROCIC!!!!!!!!!!!!!!!
-- Function used to get all data from the file                            
loadData handleFile = do isEof <- hIsEOF handleFile
	                 if isEof then
	                     return []
	                 else do
	                     line <- hGetLine handleFile
	                     rest <- (loadData handleFile)
	                     return ([(read line)] ++ rest)
	          


-- tworzy odpowiednia nazwe pliku
createFname fname = ("data/" ++ fname ++ ".data")


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
