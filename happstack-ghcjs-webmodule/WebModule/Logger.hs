{-# LANGUAGE  TypeSynonymInstances, FlexibleInstances, DefaultSignatures #-}

module WebModule.Logger where

import Control.Monad.Trans as Monad (MonadIO, liftIO)
import Data.Text as Text (Text)
import Data.Text.IO as Text (putStrLn)
import Prelude as Prelude (IO, Show(..), (.), String, putStrLn)

class Logger a where
  log :: a -> IO ()
  log' :: MonadIO m => a -> m ()

  default log :: Show a => a -> IO ()
  log = Prelude.putStrLn . show
  default log' :: (MonadIO m, Show a) => a -> m ()
  log' = liftIO . Prelude.putStrLn . show


instance Logger Text.Text where
  log = Text.putStrLn
  log' = liftIO . Text.putStrLn

instance Logger String where
  log = Prelude.putStrLn
  log' = liftIO . Prelude.putStrLn

