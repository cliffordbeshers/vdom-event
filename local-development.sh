#!/bin/bash

# Documentation on how to configure and run for development.
# The server package expects the client to be in ../happstack-ghcjs-client

set -x

# At this time, we do not attempt to build -webmodule.  Use the autobuilder and install the packages.
case $1 in
    clean)
	for d in happstack-ghcjs-client happstack-ghcjs-server ; do
	    ( cd $d ; cabal clean )
	done
	;;
    configure)
	( cd happstack-ghcjs-client ; cabal configure --ghcjs -flocal-development --ghcjs-option="-odir dist/build/tmp" --ghcjs-option="-hidir dist/build/tmp" )
	( cd happstack-ghcjs-server ; cabal configure --ghc -flocal-development )
	;;
    build)
	( cd happstack-ghcjs-client ; cabal build) && \
	( cd happstack-ghcjs-server ; cabal run)
	;;
)
	

#(cd happstack-ghcjs-client/client ; ln -sf ../../happstack-ghcjs-server/server/WebModule .)

# ( cd happstack-ghcjs-client
#   rm -rf dist
#   runhaskell Setup.hs --ghcjs configure -flocal-happstack-ghcjs-webmodule --ghcjs-option="-odir dist/build/tmp" --ghcjs-option="-hidir dist/build/tmp" && \
#   runhaskell Setup.hs build
# ) &&

# ( cd happstack-ghcjs-server
#   rm -rf dist
#   runhaskell Setup.hs configure -f local-development && \
#   runhaskell Setup.hs build --ghc-option="-odir dist/build/tmp" --ghc-option="-hidir dist/build/tmp" && \
#   dist/build/happstack-ghcjs-server/happstack-ghcjs-server
#   # runhaskell Setup.hs run
# )
