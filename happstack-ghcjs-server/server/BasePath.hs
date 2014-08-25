module BasePath where

import System.FilePath

-- Separate module because template haskell can't handle this in line.
-- But this is still wrong.  Not sure how to set this up so that
-- external resources are embedded for production but dynamically
-- reloaded for development.

basepath :: FilePath
basepath = "embedded/livedevel"
-- "/home/beshers/alldarcs/src.seereason.com/happstack-ghcjs/happstack-ghcjs-server" </> 