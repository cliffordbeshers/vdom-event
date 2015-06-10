-- requires autobuilder-seereason
import Control.Lens
import Debian.Debianize
import Debian.Debianize.Optparse (parseProgramArguments, CommandLineOptions(..))
import Debian.AutoBuilder.Details.Versions (seereasonDefaults)
import Debian.Relation (BinPkgName(..), Relation(Rel))

main :: IO ()
main =
    parseProgramArguments >>= newCabalInfo . _flags >>= evalCabalT (debianize (seereasonDefaults >> customize) >> liftCabal writeDebianization)
    where
      customize =
          do (debInfo . utilsPackageNameBase) .= Just "happstack-ghcjs-server"
             (debInfo . sourceFormat) .= Native3
             (debInfo . control . buildDepends) %= (++ [[Rel (BinPkgName "happstack-ghcjs-client") Nothing Nothing]])
