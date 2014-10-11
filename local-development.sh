#!/bin/bash

# Documentation on how to configure and run for development.
# The server package expects the client to be in ../happstack-ghcjs-client

set -x

( cd happstack-ghcjs-client
  rm -rf dist
  runhaskell Setup.hs --ghcjs configure --ghcjs-option="-odir dist/build/tmp" --ghcjs-option="-hidir dist/build/tmp"
  runhaskell Setup.hs build
)

( cd happstack-ghcjs-server
  runhaskell Setup.hs configure -f local-development
  runhaskell Setup.hs build --ghc-option="-odir dist/build/tmp" --ghc-option="-hidir dist/build/tmp"
  dist/build/happstack-ghcjs-server/happstack-ghcjs-server
  # runhaskell Setup.hs run
)