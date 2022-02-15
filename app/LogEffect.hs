{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}

module LogEffect where

-- fused effects
import Data.Kind
import Control.Algebra
import Control.Monad.IO.Class

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

data LogLevel = Inf | Dbg | Err

instance Show LogLevel where
    show :: LogLevel -> String
    show Inf = "DEBUG"
    show Dbg = "INFO"
    show Err = "ERROR"

data LogEffect (m :: Type -> Type) k where
    Log :: LogLevel -> T.Text -> LogEffect m ()

log :: Has LogEffect sig m => LogLevel -> T.Text -> m ()
log lvl txt = send (Log lvl txt)

newtype LogEffectIOC m a  = LogEffectIOC { runLogEffectIO :: m a }
    deriving (Applicative, Functor, Monad, MonadIO)

instance (MonadIO m, Algebra sig m) => Algebra (LogEffect :+: sig) (LogEffectIOC m) where
    alg hdl sig ctx = case sig of
                        L (Log lvl txt) -> ctx <$ liftIO (TIO.putStrLn $ T.append prefix content)
                            where prefix = T.pack $ show lvl ++ ": "
                                  content = T.dropWhileEnd (=='\n') txt
                        R other -> LogEffectIOC (alg (runLogEffectIO . hdl) other ctx)
