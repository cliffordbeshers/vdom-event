import Control.Category ((.))
import Debian.Debianize
import Debian.AutoBuilder.Details.CabalInfo (seereasonDefaults)
import Debian.Relation (BinPkgName(BinPkgName))
import Prelude hiding ((.))

main :: IO ()
main =
    newFlags >>= newCabalInfo >>= evalCabalT (debianize (seereasonDefaults >> customize) >> liftCabal writeDebianization)
    where
      customize =
          do (utilsPackageNameBase . debInfo) ~= Just "happstack-ghcjs-client"
             (sourceFormat . debInfo) ~= Just Native3
             (atomSet . debInfo) += InstallTo (BinPkgName "happstack-ghcjs-client")
                                              "client/Common.hs"
                                              "usr/share/happstack-ghcjs/Common.hs"
