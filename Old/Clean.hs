
import Data.Monoid
import Control.Monad
import Data.List (tails)
import Control.Concurrent.MVar (MVar, newMVar, newEmptyMVar, putMVar, takeMVar)

data State = State Int deriving Show
data VNode = VNode Int
data Patch = Patch Int
data DOMNode = DOMNode Int

instance Monoid VNode where
  mempty = VNode 0
  (VNode a) `mappend` (VNode b) = VNode (a + b)

render :: State -> VNode
render (State s) = VNode s

step :: State -> State
step (State s) = State (s+1)

ss :: [State]
ss = iterate step (State 0)

ts :: [VNode]
ts = map render ss

diff :: VNode -> VNode -> Patch
diff (VNode a) (VNode b) = Patch (b - a)

patch :: DOMNode -> Patch -> IO DOMNode
patch (DOMNode n) (Patch d) = return (DOMNode (n+d))

-- Zip a infinite list with itself, offset by one.
offset :: [a] -> [(a,a)]
offset xs = [(x,y) | (x:y:_) <- tails xs ]

zzz :: State -> [Patch]
zzz = map (uncurry diff) . offset . (mempty :) . map render . iterate step

ddd :: DOMNode -> [Patch] -> IO DOMNode
ddd = foldM patch

app :: DOMNode -> State -> IO DOMNode
app root = ddd root . zzz

-- where do events fit in?

type Buffer a = MVar a
buffer :: a -> IO (Buffer a)
buffer s = do
  buf <- newMVar s
  return buf

yield :: MVar State -> State -> IO ()
yield = putMVar

block :: MVar State -> IO State
block = takeMVar


