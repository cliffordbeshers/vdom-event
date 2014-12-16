{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -fno-warn-orphans #-}

#if CLIENT
module WebModule.EmbedURI where
#endif
#if SERVER
module WebModule.EmbedURI (embedURI, embedRelativeURI, showURI, putURI, putURILn) where

import Data.List (intercalate)
import Data.Maybe (fromJust)
import Language.Haskell.TH (Exp, Q)
import Language.Haskell.TH.Lift (deriveLift, Lift(lift))
import "network-uri" Network.URI (parseRelativeReference, parseURI, URI(uriAuthority, uriFragment, uriPath, uriQuery, uriScheme), URIAuth)

$(deriveLift ''URIAuth)
$(deriveLift ''URI)

embedURI :: String -> Q Exp -- URI
embedURI = lift . fromJust . parseURI

embedRelativeURI :: String -> Q Exp -- URI
embedRelativeURI = lift . fromJust . parseRelativeReference

-- a URI show function that shows the record structure
showURI :: URI -> String
showURI u = "URI {" ++ (commas $ fields u) ++ "}"
  where fields v = zipWith (\n x -> n ++ " = " ++ x) names (values v)
        commas = intercalate ", "
        names = [ "uriScheme", "uriAuthority", "uriPath", "uriQuery", "uriFragment"]
        values uu = [ show $ uriScheme uu
                    , show $ uriAuthority uu
                    , show $ uriPath uu
                    , show $ uriQuery uu
                    , show $ uriFragment uu
                    ]
  
putURI :: URI -> IO ()
putURI = putStr . showURI

putURILn :: URI -> IO ()
putURILn = putStrLn . showURI
#endif