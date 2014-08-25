{-# LANGUAGE CPP #-}

#ifdef ghcjs_HOST_OS
#define SERVER 0
#define CLIENT 1
#else
#define SERVER 1
#define CLIENT 0
#endif

module WImport where

import Control.Monad.Trans.Writer

type Bindings = Int

data WebModule = WebModule { bindings :: Bindings }

type WebM m a = WriterT [WebModule] m a

wimport :: Monad m => WebModule -> WebM m Bindings
wimport mod = do
  tell [mod]
  return (bindings mod)

-- Not even sure I need to do this.
-- #if CLIENT
-- wimport :: WebModule a -> a
-- #else
-- wimport :: WebModule a -> a
-- #endif

-- On the client side,
-- I just take the return value, the computation to be run as the application.

-- On the server side, I throw away that computation and just keep the module list
-- From that list I generate index.html with all of the references to the resources, plus an msum of serverparts that serve those resources.