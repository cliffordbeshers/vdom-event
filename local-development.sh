#!/bin/bash

# Documentation on how to configure and run for development.
# The server package expects the client to be in ../happstack-ghcjs-client

set -x

( cd happstack-ghcjs-client
  rm -rf dist
  runhaskell Setup.hs --ghcjs configure
  runhaskell Setup.hs build
)

( cd happstack-ghcjs-server
  runhaskell Setup.hs configure -f local-development
  runhaskell Setup.hs build
  dist/build/happstack-ghcjs-server/happstack-ghcjs-server
  # runhaskell Setup.hs run
)