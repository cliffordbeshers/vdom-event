-- requires autobuilder-seereason
import Control.Category ((.))
import Debian.Debianize
import Debian.AutoBuilder.Details.CabalInfo (seereasonDefaults)
import Debian.Relation (BinPkgName(..), Relation(Rel))
import Prelude hiding ((.))

main :: IO ()
main =
    newFlags >>= newCabalInfo >>= evalCabalT (debianize (seereasonDefaults >> customize) >> liftCabal writeDebianization)
    where
      customize =
          do (utilsPackageNameBase . debInfo) ~= Just "happstack-ghcjs-server"
             (sourceFormat . debInfo) ~= Just Native3
             (buildDepends . control . debInfo) %= (++ [[Rel (BinPkgName "happstack-ghcjs-client") Nothing Nothing]])
