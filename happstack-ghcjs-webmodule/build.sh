#!/bin/bash

sudo apt-get remove -y libghcjs-happstack-ghcjs-webmodule-dev libghcjs-happstack-ghcjs-webmodule-doc  libghc-happstack-ghcjs-webmodule-dev libghc-happstack-ghcjs-webmodule-doc

runhaskell debian/Debianize.hs && dpkg-buildpackage && sudo debi && \
runhaskell debian/Debianize.hs --ghcjs && dpkg-buildpackage && sudo debi

