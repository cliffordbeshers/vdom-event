#!/bin/bash

# Documentation on how to configure and run for development.
# The server package expects the client to be in ../happstack-ghcjs-client

set -x

( cd ..
#  rm -rf dist
  runhaskell Setup.hs --ghcjs configure --ghcjs-option="-odir dist/build/tmp" --ghcjs-option="-hidir dist/build/tmp" && \
  runhaskell Setup.hs build
)