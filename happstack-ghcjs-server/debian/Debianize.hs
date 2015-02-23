-- requires autobuilder-seereason
import Control.Lens hiding ((%=))
import Debian.Debianize
import Debian.AutoBuilder.Details.Versions (seereasonDefaults)
import Debian.Relation (BinPkgName(..), Relation(Rel))

main :: IO ()
main =
    newFlags >>= newCabalInfo >>= evalCabalT (debianize (seereasonDefaults >> customize) >> liftCabal writeDebianization)
    where
      customize =
          do (debInfo . utilsPackageNameBase) ~= Just "happstack-ghcjs-server"
             (debInfo . sourceFormat) ~= Just Native3
             (debInfo . control . buildDepends) %= (++ [[Rel (BinPkgName "happstack-ghcjs-client") Nothing Nothing]])
