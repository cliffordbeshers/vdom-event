import Control.Category ((.))
import Debian.Debianize
import Distribution.Compiler
import Data.Lens.Lazy hiding ((~=), (%=))
import Debian.Relation
import Prelude hiding ((.))

main :: IO ()
main = newFlags >>= newCabalInfo >>= evalCabalT (debianize (debianDefaults >> customize) >> liftCabal writeDebianization)

customize :: CabalT IO ()
customize = do
  hc <- access (compilerFlavor . flags . debInfo)
  case hc of
    GHC -> (sourceFormat . debInfo) ~= Just Native3
    GHCJS ->
        do let (Right rels) = parseRelations "ghcjs, libghc-cabal-ghcjs-dev, haskell-devscripts (>= 0.8.21.3)"
           (sourcePackageName . debInfo) ~= Just (SrcPkgName "ghcjs-happstack-ghcjs-webmodule")
           (sourceFormat . debInfo) ~= Just Native3
           (buildDepends . control . debInfo) %= (++ rels)
