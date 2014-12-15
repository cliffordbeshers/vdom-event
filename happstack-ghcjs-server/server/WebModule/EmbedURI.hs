{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}
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
  where fields v = zipWith (\n v -> n ++ " = " ++ v) names (values v)
        commas = intercalate ", "
        names = [ "uriScheme", "uriAuthority", "uriPath", "uriQuery", "uriFragment"]
        values u = [ show $ uriScheme u
                   , show $ uriAuthority u
                   , show $ uriPath u
                   , show $ uriQuery u
                   , show $ uriFragment u
                   ]
  
putURI :: URI -> IO ()
putURI = putStr . showURI

putURILn :: URI -> IO ()
putURILn = putStrLn . showURI
