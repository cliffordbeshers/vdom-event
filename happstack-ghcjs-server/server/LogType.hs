module LogType where

import Happstack.Server.Internal.LogFormat (formatRequestCombined)
import System.Log.Handler.Simple (fileHandler, streamHandler)
import System.Log.Logger (debugM, logM, Priority(ALERT, DEBUG, INFO), rootLoggerName, setHandlers, setLevel, updateGlobalLogger)
import Data.Time.Format (FormatTime(..))
import Happstack.Server
import System.FilePath

data LogMode
    = Production
    | Development
    deriving (Read, Show, Eq, Ord, Enum, Bounded)

logMAccess' :: FormatTime t => LogAccess t
logMAccess' host user time requestLine responseCode size referer userAgent =
    logM "SimpleServer" DEBUG $ formatRequestCombined host user time requestLine responseCode size referer "" -- userAgent

