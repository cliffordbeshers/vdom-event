module IterateM (iterateM, iterateNM) where

-- Source
-- http://stackoverflow.com/questions/7928005/haskell-iterate-in-state-how-to-force-the-behaviour-i-want
  
iterateNM :: (Monad m) => Int -> (a -> m a) -> m a -> m [a]
iterateNM k _ _ | k < 0 = error "Model.IterateM.iterateNM given negative count"
iterateNM 0 _ _ = return []
iterateNM k step start = do
    first <- start
    rest <- iterateNM (k-1) step (step first)
    return (first:rest)


-- Never terminates
iterateM :: Monad m => (a -> m a) -> m a -> m [a]
iterateM step start = do
    first <- start
    rest <- iterateM step (step first)
    return (first:rest)
    
