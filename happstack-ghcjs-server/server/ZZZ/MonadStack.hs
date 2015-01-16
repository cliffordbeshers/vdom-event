{-# LANGUAGE PackageImports #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell #-}
module Clckwrks.AVR3.ErrorMonad where

import Clckwrks.AVR3.Bootstrap.Blaze as H (Markup, ToMarkup(toMarkup))
import Clckwrks.AVR3.ErrorPage (ErrorPage(E_Message))
import Clckwrks.AVR3.WithLocation (withLocation)
import Control.Monad.Trans.Error (catchError, ErrorT(runErrorT), throwError)
import Control.Monad.Trans.Reader (mapReaderT, ReaderT(runReaderT))
import Happstack.Server (escape, FilterMonad, ok, Response, ToMessage(toResponse), WebMonad)
--import Clckwrks.AVR3.ErrorMonad (renderErrorPage)

-- x = do
x :: (FilterMonad Response m, WebMonad Response m) => m a
x = escape e

e :: FilterMonad Response m => m Response
e = ok $ toResponse "fail"

-- t0 :: MonadError String Maybe  => m String
t0 :: Monad m => ErrorT String m (Either String b)
t0 = throwError "hello" `catchError` (\e -> return $ Left e)

t1 :: Monad m => Maybe Int -> ErrorT ErrorPage m H.Markup
t1 ms = f ms `catchError` (\e -> return $ toMarkup e)
  where f Nothing = throwError $(withLocation [|E_Message|])
        f (Just s) = return $ toMarkup s

-- t2 :: IO (Either ErrorPage H.Markup)
-- t2 :: Monad m => m Markup
runErrorMarkup :: (Monad m, ToMarkup a) => (ErrorT a m Markup) -> m Markup
runErrorMarkup em = do
  ee <-runErrorT $ em
  return $ either toMarkup Prelude.id ee

renderErrorPage = either toMarkup Prelude.id

-- foo = $withFileLine $ [| (++ "My message") |]
bar s = s ++ "My message"
foo = $(withLocation  [| (++ "fooooo") |])


zzz :: b -> ReaderT b m a -> m a
zzz r = flip runReaderT r

eee :: Functor m => (Either e a -> a) -> b -> ReaderT b (ErrorT e m) a -> m a
eee f r = flip runReaderT r . mapReaderT (fmap f . runErrorT)
  -- where resolveError :: Either e a -> a
  --       resolveError = undefined
  --       undefined = undefined

-- runAVR3TE :: Functor m => AVR3Config -> AVR3TE AVR3URL m a -> ClckT AVR3URL m a
-- runAVR3TE r m = mapClckT (eee f r) m
--   where f (Left e) = return $ (either toMarkup Prelude.id e, s) -- where do I get the state from here?  I don't think I can use mapClckT
--         f (Right (a,s)) = return $ (a, s)

--  where f' r m = runReaderT (runErrorT m) r
-- runAVR3TE mc m = mapClckT f' m
--   where f' = f mc


--       f r m = undefined -- (runReaderT r mc) . (mapReaderT runE) $ m
--       runE m = either toMarkup MyPrelude.id <$> runErrorT m
--       undefined = undefined

