import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Reader
import Data.Supply


type Ident = Int
type IdentSupply = Supply Ident
data Config = Config { identSupply :: IdentSupply }

data Widget = Widget { ident :: Ident }

-- Global Monad
type GM = ReaderT Config Identity

runGM :: GM a -> Config -> a
runGM m r = runIdentity (runReaderT m r)

mkConfig :: IO Config
mkConfig = Config <$> newEnumSupply

genIds :: GM [Int]
genIds = (map supplyValue . split . identSupply) <$> ask

a :: GM Widget
a = do
  i:_ <- genIds
  return $ Widget { ident = i }

b :: GM Widget
b = do
  i:_ <- genIds
  return $ Widget { ident = i }
