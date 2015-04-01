import Control.Applicative ((<$>))
import Control.Lens hiding ((+=))
import Debian.Debianize
import Debian.AutoBuilder.Details.Versions (seereasonDefaults)
import Debian.Relation (BinPkgName(BinPkgName))
import Distribution.Compiler (CompilerFlavor(GHCJS))
import System.FilePath ((</>))
import System.FilePath.Find

main :: IO ()
main = do
  hsFiles <- concat <$> (mapM (find always (extension ==? ".hs")) ["client", "GHCJSStub"])
  newFlags >>= newCabalInfo >>= evalCabalT (debianize (seereasonDefaults >> customize hsFiles) >> liftCabal writeDebianization)
    where
      customize hsFiles =
          do (debInfo . utilsPackageNameBase) ~= Just "happstack-ghcjs-client"
             (debInfo . sourceFormat) ~= Just Native3
             (debInfo . flags . compilerFlavor) ~= GHCJS
             -- cabal-debian currently can't correctly install the
             -- names listed in data-files, so list these explicitly.
             mapM_ dataFile hsFiles

      dataFile path = (debInfo . atomSet) += InstallTo (BinPkgName "happstack-ghcjs-client") path ("usr/share/happstack-ghcjs-client" </> path)
