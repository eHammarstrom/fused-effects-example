{-# LANGUAGE OverloadedStrings #-}

module File where

import Control.Exception (catch, IOException)
import GHC.IO.Exception (ioe_type, IOErrorType(NoSuchThing))
import Data.Functor
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

type File = Either FileError (FilePath, T.Text)

data FileError = GenericErr FilePath
               | FileNotFound FilePath

-- TODO: Use LogEffect to log error
printFileError :: FileError -> IO ()
printFileError (GenericErr fp) = TIO.putStrLn (T.append (T.pack fp) ": Unable to read file")
printFileError (FileNotFound fp) = TIO.putStrLn (T.append (T.pack fp) ": No such file or directory")

printFile :: File -> IO ()
printFile (Left err) = printFileError err
printFile (Right (fp, txt)) = TIO.putStrLn $ T.append (T.append (T.pack fp) ": ") txt

readFileMaybe :: FilePath -> IO File
readFileMaybe f = catch tryHandler errHandler
    where
        tryHandler :: IO File
        tryHandler = TIO.readFile f <&> prependFilePath <&> Right
        errHandler :: IOException -> IO File
        errHandler exc = case ioe_type exc of
                           NoSuchThing -> return . Left $ FileNotFound f
                           _           -> return . Left $ GenericErr f
        prependFilePath txt = (f, txt)
