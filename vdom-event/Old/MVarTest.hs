import Control.Concurrent
import Control.Monad

thread :: String -> MVar Int  -> MVar Int  -> IO ()
thread name mine his = forever $ do
  print (name, "waiting")
  s <- takeMVar mine
  print (name, "tookMVar mine", s)
  let t = truncate 1e6
  print (name, "sleeping", (fromIntegral t/1e6))
  threadDelay t
  print (name, "before putMVar his", s+1)
  putMVar his (s+1)

thread2 :: String -> IO Int  -> (Int -> IO ())  -> IO ()
thread2 name mine his = forever $ do
  print (name, "waiting")
  s <- mine
  print (name, "tookMVar mine", s)
  let t = truncate 1e6
  print (name, "sleeping", (fromIntegral t/1e6))
  threadDelay t
  print (name, "before putMVar his", s+1)
  his (s+1)


main1 = do
  x <- newMVar 0
  y <- newEmptyMVar
  t2 <- forkIO $ thread "    y" y x
  threadDelay $ truncate 2e6
  t1 <- forkIO $ thread "x" x y
  threadDelay $ truncate 10e6

main2 = do
  x <- newMVar 0
  y <- newEmptyMVar
  t2 <- forkIO $ thread2 "    y" (takeMVar y) (putMVar x)
  threadDelay $ truncate 2e6
  t1 <- forkIO $ thread2 "x" (takeMVar x) (putMVar y)
  threadDelay $ truncate 10e6

main = main2
