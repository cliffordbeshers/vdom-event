import Control.Monad.State (get)
import Control.Lens
import Debian.Debianize
import Distribution.Compiler
import Debian.Relation

main :: IO ()
main = performDebianization (debianDefaults >> customize)

customize :: CabalT IO ()
customize = do
  hc <- get >>= return . view (debInfo . flags . compilerFlavor)
  case hc of
    GHC -> (debInfo . sourceFormat) .= Native3
    GHCJS ->
        do let (Right rels) = parseRelations "ghcjs, libghc-cabal-122-dev, haskell-devscripts (>= 0.8.21.3)"
           (debInfo . sourcePackageName) .= Just (SrcPkgName "ghcjs-happstack-ghcjs-webmodule")
           (debInfo . sourceFormat) .= Native3
           (debInfo . control . buildDepends) %= (++ rels)
