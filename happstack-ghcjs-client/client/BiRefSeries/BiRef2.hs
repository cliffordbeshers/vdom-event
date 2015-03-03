import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Reader
-- import Data.Supply
import Data.Bimap
import System.IO.Unsafe
import ControlMonadSupply


type Ident = Int
type IdentSupply = Supply Ident
data Config = Config { identSupply :: IdentSupply }

data Widget = Widget { ident :: Ident }

-- Global Monad
type GM m = ReaderT Config (StateT IdentSupply m ())

gm :: GM a -> IO a
gm m = runGM m =<< mkConfig

runGM :: GM a -> Config -> IO a
runGM m r = runReaderT m r

mkConfig :: IO Config
mkConfig = Config <$> (newDupableSupply (toEnum 0) succ)

genIds :: GM [Int]
genIds = (map supplyValue . split . identSupply) <$> ask

gen :: Config -> [Ident]
gen = map supplyValue . split . identSupply

a :: GM Widget
a = do
  i:_ <- gen <$> ask
  return $ Widget { ident = i }

b :: GM Widget
b = do
  i:_ <-  gen <$> ask
  return $ Widget { ident = i }

type DOM = [Widget]


type Ref = (Ident, Ident)
ref = (,)
-- biRef = Widget -> Widget -> GM -- _biRef = undefined

biref a b = [(a,b), (b,a)]
birefW a b = [(ident a,ident b), (ident b,ident a)]

dom :: GM [Ref]
dom = do
  a' <- a
  b' <- b
  return $ birefW a' b'

test :: GM [Ident]
test = do
  s0 <- liftIO newNumSupply
  let (s1,s2,s3) = split3 s0
  let ss = split s2
  return [supplyValue s0, supplyValue s1, supplyValue s2]

test2 :: GM [Ident]
test2 = do
  s0 <- identSupply <$> ask
  let (s1,s2,s3) = split3 s0
  let ss = split s2
  return [supplyValue s0, supplyValue s1, supplyValue s2]

test3 = do
  xs <- test2
  ys <- test2
  return $ xs ++ ys

--  print' $ take 5 $ map supplyValue ss
--  print' $ take 5 $ map supplyValue (split s1)
  -- let s4 = modifySupply s3 (("prefix" ++) . show . supplyValue)
  -- let (s5,s6) = split2 s4
  -- print' $ supplyValue s4
  -- print' $ take 5 $ map supplyValue (split s5)
  -- print' $ take 5 $ map supplyValue (split s6)

prefixSupply :: Show a => String -> Supply a -> Supply String
prefixSupply prf sup = modifySupply sup ((prf ++) . show . supplyValue)

newPrefixSupply :: String -> IO (Supply String)
newPrefixSupply prefix = do
  s0 <- newNumSupply
  return $ prefixSupply prefix s0
