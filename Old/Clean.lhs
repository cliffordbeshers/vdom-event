
Necessary imports.

> import Data.Monoid
> import Control.Monad
> import Data.List (tails)
> import Control.Concurrent.MVar (MVar, newMVar, newEmptyMVar)

Simple versions of the datatypes we expect in a real application.

State here is the appilication state, which is displayed by running
'render' to produce a value of type VNode.  VNode is a Haskell data
structure that parallels the browser's DOM model.

The function 'diff' compares to VNode's and returns a Patch, which can
be applied to an actual DOMNode to change the state of the DOM in the
browser.

> data State = State Int deriving Show
> data VNode = VNode Int
> data Patch = Patch Int
> data DOMNode = DOMNode Int

I'm not sure that DOM trees are actually monoids.  I really just
wanted to represent an empty value.  Perhaps the empty tree forms some
sort of monoid with Patch?

> instance Monoid VNode where
>   mempty = VNode 0
>   (VNode a) `mappend` (VNode b) = VNode (a + b)

> render :: State -> VNode
> render (State s) = VNode s

> step :: State -> State
> step (State s) = State (s+1)

> ss :: [State]
> ss = iterate step (State 0)

> ts :: [VNode]
> ts = map render ss

> diff :: VNode -> VNode -> Patch
> diff (VNode a) (VNode b) = Patch (b - a)

> patch :: DOMNode -> Patch -> IO DOMNode
> patch (DOMNode n) (Patch d) = return (DOMNode (n+d))

> -- Zip a infinite list with itself, offset by one.
> offset :: [a] -> [(a,a)]
> offset xs = [(x,y) | (x:y:_) <- tails xs ]

> zzz :: State -> [Patch]
> zzz = map (uncurry diff) . offset . (mempty :) . map render . iterate step

> ddd :: DOMNode -> [Patch] -> IO DOMNode
> ddd = foldM patch

> app :: DOMNode -> State -> IO DOMNode
> app root = ddd root . zzz

Where do events fit in?  Continuations might be nice here, but I will try that later.

The essence of the propose mechanism is to have two mvars acting as
work queues.  The 'dirty' mvar holds state not yet rendered.  After
the DOM is patched, the current state should be written to the 'clean'
mvar.  Any event handler triggered should read the clean state and
write an updated version back to 'dirty'.  This should unblock the
application thread an render the new scene.

> type Buffer a = (MVar a, MVar a)
> buffer :: a -> IO (Buffer a)
> buffer s = do
>   dirty <- newMVar s
>   clean <- newEmptyMVar
>   return (dirty, clean)


What does an event handler look like? Just handle clicks, for now.  We
need to identify a node, because handlers get attached to actual DOMNode's.

An event handler is given an Event (a DOM type) and it should produce
a new State value. To produce a new State, it needs the old state.

> -- handler :: Buffer State -> Event -> IO (Buffer State)
> -- handler = undefined

> -- click :: DOMNode -> (EventHandler State) -> IO State
> -- click = undefined

> main  = app undefined undefined
