module Tasks where

-- TODO "happstack-ghcjs-client" Tasks.TaskManagerInterface
-- A UI for talking to David's task manager

-- Not built for GHCJS yet, not sure it should be.  Build it here and find out.
-- import System.Tasks

data TaskRequest =
  TaskStart |
  TaskTest123 |
  TaskTestError |
  TaskStatus (Maybe Int) |
  TaskKill Int |
  TaskShutDown

data TaskResponse =
  

keyboard :: StateT ([MVar ToTask -> IO ()], TID) IO ToManager
keyboard = do
  input <- lift $ getLine
  case input of
    -- Start a new task
    "t" -> get >>= \ ((cmd : more), nextId) -> put (more, succ nextId) >> return (TopToManager (StartTask nextId cmd))
    -- Start an IO 123
    "a" -> get >>= \ (cmds, nextId) -> put (cmds, succ nextId) >> return (TopToManager (StartTask nextId (runIO (return 123))))
    -- Throw an exception
    "e" -> get >>= \ (cmds, nextId) -> put (cmds, succ nextId) >> return (TopToManager (StartTask nextId (runIO (throw LossOfPrecision))))
    -- Get the status of a task
    ['s',d] | isDigit d -> return (TopToManager (SendTaskStatus (read [d])))
    -- Get process manager status
    "s" -> return (TopToManager SendManagerStatus)
    -- Kill a task
    ['k',d] | isDigit d -> return (TopToManager $ TopToTask $ CancelTask $ read [d])
    -- Initiate shutdown and exit keyboard loop
    "x" -> return (TopToManager ShutDown)
    -- error
    x -> ePutStrLn (show x ++ " - expected: t, a, e, s, s<digit>, k<digit>, or x") >> keyboard
