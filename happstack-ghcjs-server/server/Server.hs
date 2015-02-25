{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}
module Main where

-- TODO "happstack-ghcjs-server" Main.ReplaceServerHSWithClientServerFromClient
-- This file should not exist.  The cabal file should point to a unified ClientServer.hs
-- in -client so that the application is always compiled from one code tree.

-- TODO "happstack-ghcjs-server" WebModule.WebModule.RepresentPageStateMonad

import Control.Monad (MonadPlus(..))
import Happstack.Server
import WebModule.Favicon
import WebModule.Markable
import WebModule.WebModule
import WebModule.WebModuleM
import WebModule.JQueryWebModule
import WebModule.JQueryUIWebModule
import WebModule.BootstrapWebModule
import WebModule.GHCJSWebModule
import WebModule.SortableModule
import WebModule.ServeEmbedded (EmbeddedDirectory, embedDirectoryTH)

import ClientServer (clientServerWebSite)

main :: IO ()
main = do
  let p = 8010
  print ("Serving on localhost",p)
  (_, ws) <- compileWebSiteM clientServerWebsite
  simpleHTTP (nullConf { port = p }) $ serverpart ws
  
