#!/bin/bash

# Documentation on how to configure and run for development.
# The server package expects the client to be in ../happstack-ghcjs-client

set -x

( cd happstack-ghcjs-client
  cabal --ghcjs configure
  cabal build
)

( cd happstack-ghcjs-server
  cabal -f local-development configure
  cabal build
  cabal run
)