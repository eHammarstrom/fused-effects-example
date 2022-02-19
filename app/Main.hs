{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Prelude hiding (readFile)

import System.Environment (getArgs)
import System.Exit (exitFailure)

import Control.Carrier.Lift
import Control.Algebra
import qualified Data.Text as T

import File
import FileEffect
import LogEffect

processFile :: (Has LogEffect sig m, Has FileEffect sig m) => FilePath -> m ()
processFile path = do
    file <- readFile path
    printFile file

main :: IO ()
main = do
    args <- getArgs
    mapM_ (runFileEffectIO . runLogEffectIO . processFile) args
