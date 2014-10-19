module EmbedGHCJS where

import Happstack.Server
import LogType
import Control.Monad.Trans as Trans (liftIO)
import Control.Monad as Monad (msum)
import System.FilePath
import System.Log.Logger (debugM, logM, Priority(ALERT, DEBUG, INFO), rootLoggerName, setHandlers, setLevel, updateGlobalLogger)


type GHCJSPackageName = String

--rootHandler :: ServerPartT IO Response
--rootHandler = msum [ nullDir >> ok (toResponse application)
--                   , dirs "index.html" $ ok (toResponse application)
--                   ]

ghcjsHandler :: LogMode -> GHCJSPackageName -> ServerPartT IO Response
ghcjsHandler mode package = do
  dirs "/" $  msum $ [ foo ] ++ map servejs ["/lib.js", "/rts.js", "/lib1.js", "/out.js" ]
  where servejs fp = do
          liftIO $ logM "Server.hs/ghcjsHandler" ALERT $ "Serving now <<<" ++ (basepath' package </> fp) ++ ">>>"
          dirs fp $ serveFile (asContentType "application/javascript") $ (basepath' package) </> fp
          ok $ toResponse "ghcjsHandler failed"
        foo = dirs "/index.html" $ do liftIO $ putStrLn "Server.hs/ghcjsHandler" >> putStrLn ("Serving / with "  ++ (basepath' package </> "index.html"))
                                      serveFile (asContentType "text/html") $ basepath' package </> "index.html"
        basepath' p = 
          case mode of
            Production -> "/usr/bin" </> (p <.> "jsexe")
            Development -> "/home/beshers/alldarcs/src.seereason.com/happstack-ghcjs/" </> p </> "dist/build" </> p </> (p <.> "jsexe")
                        

-- ghcjsHandler :: LogMode -> GHCJSPackageName -> ServerPartT IO Response
-- ghcjsHandler mode package =
--   msum $ [ foo ] ++ map servejs ["/lib.js", "/rts.js", "/lib1.js", "/out.js" ]
--   where servejs fp = do
--           liftIO $ logM "Server.hs/ghcjsHandler" ALERT $ "Serving now <<<" ++ (basepath' package </> fp) ++ ">>>"
--           dirs fp $ serveFile (asContentType "application/javascript") $ (basepath' package) </> fp
--           ok $ toResponse "ghcjsHandler failed"
--         foo = dirs "/index.html" $ do liftIO $ putStrLn "Server.hs/ghcjsHandler" >> putStrLn ("Serving / with "  ++ (basepath' package </> "index.html"))
--                                       serveFile (asContentType "text/html") $ basepath' package </> "index.html"
--         basepath' p = 
--           case mode of
--             Production -> "/usr/bin" </> (p <.> "jsexe")
--             Development -> "/home/beshers/alldarcs/src.seereason.com/happstack-ghcjs/" </> p </> "dist/build" </> p </> (p <.> "jsexe")
                        