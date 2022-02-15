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

main :: IO ()
main = do
    args <- getArgs
    files <- runFileEffectIO (readFiles args)
    mapM_ (runLogEffectIO . printFile) files
