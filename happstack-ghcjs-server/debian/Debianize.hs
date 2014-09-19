import Debian.Debianize
import Debian.AutoBuilder.Details.Atoms (seereasonDefaultAtoms)
import Debian.Relation (BinPkgName(..), Relation(Rel))

main :: IO ()
main =
    newAtoms >>= evalDebT (debianization seereasonDefaultAtoms customize >> writeDebianization)
    where
      customize =
          do utilsPackageNameBase ~= Just "happstack-ghcjs-server"
             sourceFormat ~= Just Native3
             buildDepends %= (++ [[Rel (BinPkgName "happstack-ghcjs-client") Nothing Nothing]])
