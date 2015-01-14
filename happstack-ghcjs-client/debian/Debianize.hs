import Debian.Debianize
import Debian.AutoBuilder.Details.Atoms (seereasonDefaultAtoms)
import Debian.Relation (BinPkgName(BinPkgName))

main :: IO ()
main =
    newAtoms >>= evalDebT (debianize (seereasonDefaultAtoms >> customize) >> writeDebianization)
    where
      customize =
          do utilsPackageNameBase ~= Just "happstack-ghcjs-client"
             sourceFormat ~= Just Native3
             installTo (BinPkgName "happstack-ghcjs-client")
                       "client/Common.hs"
                       "usr/share/happstack-ghcjs/Common.hs"
