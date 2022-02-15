{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module File where

import Control.Exception (catch, IOException)
import GHC.IO.Exception (ioe_type, IOErrorType(NoSuchThing))
import Data.Functor
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- fused-effects
import Data.Kind
import Control.Algebra
import Control.Monad.IO.Class

import Prelude hiding (log)
import LogEffect

type File = Either FileError (FilePath, T.Text)

data FileError = GenericErr FilePath
               | FileNotFound FilePath

printFile :: Has LogEffect sig m => File -> m ()
printFile (Left (GenericErr fp)) = log Err (T.append (T.pack fp) ": Unable to read file")
printFile (Left (FileNotFound fp)) = log Err (T.append (T.pack fp) ": No such file or directory")
printFile (Right (fp, txt)) = log Inf (T.append (T.append (T.pack fp) ": ") txt)

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
