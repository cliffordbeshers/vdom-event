import Debian.Debianize
import Debian.AutoBuilder.Details.Atoms (seereasonDefaultAtoms)
import Debian.Relation (BinPkgName(BinPkgName))

main :: IO ()
main =
    newAtoms >>= evalDebT (debianization seereasonDefaultAtoms customize >> writeDebianization)
    where
      customize =
          do utilsPackageNameBase ~= Just "happstack-ghcjs-client"
             installTo (BinPkgName "happstack-ghcjs-client")
                       "client/Common.hs"
                       "usr/share/happstack-ghcjs/Common.hs"
