
data BrowserHistory s =
  Back | Forward | Go Int | Length |
  PushState s (Maybe Text)  (Maybe URL) |
  PopState s |
  ReplaceState s

-- Is there a way to query the current position?
-- I don't see it.

-- Could this be a free monad?
