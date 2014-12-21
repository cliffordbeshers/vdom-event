-- This should be replaced with WebRoutes at some point, just too much cognitive overload to switch everything over to that monadic structure right now.
{-# LANGUAGE TypeFamilies, StandaloneDeriving, FlexibleContexts  #-}
{-# OPTIONS -Wall -fno-warn-orphans #-}

module AppraisalScribe.MyURL where

import Network.URI

class ToURL a where
    type URLT a
    toURL :: a -> URLT a
    nullURL :: a
    errorURL :: a -> URLT a

--    toString :: Show URL => a -> String
--    nest :: a -> a -> a

printURI :: Maybe URI -> IO ()
printURI Nothing = return ()
printURI (Just uri) =
   mapM_ ($ uri) [print . uriScheme, print . uriAuthority, print . uriPath, print . uriQuery, print . uriFragment]
