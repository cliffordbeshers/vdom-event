import Control.Lens hiding ((+=))
import Debian.Debianize
import Debian.AutoBuilder.Details.Versions (seereasonDefaults)
import Debian.Relation (BinPkgName(BinPkgName))

main :: IO ()
main =
    newFlags >>= newCabalInfo >>= evalCabalT (debianize (seereasonDefaults >> customize) >> liftCabal writeDebianization)
    where
      customize =
          do (debInfo . utilsPackageNameBase) ~= Just "happstack-ghcjs-client"
             (debInfo . sourceFormat) ~= Just Native3
             (debInfo . atomSet) += InstallTo (BinPkgName "happstack-ghcjs-client")
                                              "client/Common.hs"
                                              "usr/share/happstack-ghcjs/Common.hs"
