module DataStorage (loadBook,overwriteBook) where

import Data.List
import System.IO
import System.Directory
import Terminal
import Phonebook

tempFile = "temp"

-- ladowanie phonebook z pliku
loadBook :: [Char] -> IO Phonebook
loadBook fname =   do   System.Directory.createDirectoryIfMissing True "data/"
                        handleFile <- openFile (createFname fname) ReadWriteMode
                        idata <- loadData handleFile 
                        return idata

-- odczytuje zawartość z pliku (ktory jest przekazny przez uchwyt)
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
		      

-- tworzy odpowiednia nazwe pliku
createFname fname = ("data/" ++ fname ++ ".data")
