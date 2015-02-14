import Control.Monad.State (get)
import Control.Lens hiding ((%=))
import Debian.Debianize
import Distribution.Compiler
import Debian.Relation

main :: IO ()
main = newFlags >>= newCabalInfo >>= evalCabalT (debianize (debianDefaults >> customize) >> liftCabal writeDebianization)

customize :: CabalT IO ()
customize = do
  hc <- get >>= return . view (debInfo . flags . compilerFlavor)
  case hc of
    GHC -> (debInfo . sourceFormat) ~= Just Native3
    GHCJS ->
        do let (Right rels) = parseRelations "ghcjs, libghc-cabal-ghcjs-dev, haskell-devscripts (>= 0.8.21.3)"
           (debInfo . sourcePackageName) ~= Just (SrcPkgName "ghcjs-happstack-ghcjs-webmodule")
           (debInfo . sourceFormat) ~= Just Native3
           (debInfo . control . buildDepends) %= (++ rels)
