{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module FileEffect where

import Prelude hiding (readFile)

-- fused effects
import Data.Kind
import Control.Algebra
import Control.Monad.IO.Class

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- local module
import File

data FileEffect (m :: Type -> Type) k where
    ReadFile  :: FilePath -> FileEffect m File
    WriteFile :: FilePath -> T.Text -> FileEffect m ()

readFile :: Has FileEffect sig m => FilePath -> m File
readFile fp = send (ReadFile fp)

readFiles :: Has FileEffect sig m => [FilePath] -> m [File]
readFiles = mapM readFile

writeFile :: Has FileEffect sig m => FilePath -> T.Text -> m ()
writeFile fp txt = send (WriteFile fp txt)

newtype FileEffectIOC m a  = FileEffectIOC { runFileEffectIO :: m a }
    deriving (Applicative, Functor, Monad, MonadIO)

instance (MonadIO m, Algebra sig m) => Algebra (FileEffect :+: sig) (FileEffectIOC m) where
    alg hdl sig ctx = case sig of
                        L (ReadFile fp) -> (<$ ctx) <$> liftIO (readFileMaybe fp)
                        L (WriteFile fp txt) -> ctx <$ liftIO (TIO.writeFile fp txt)
                        R other -> FileEffectIOC (alg (runFileEffectIO . hdl) other ctx)
