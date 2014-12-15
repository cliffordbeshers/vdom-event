{-# LANGUAGE TemplateHaskell #-}
module WebModule.WithLocation (withLocation, moduleNameTH) where
import Language.Haskell.TH
import Language.Haskell.TH.Lift

-- withLocation' :: String -> IO a -> IO a
-- withLocation' s f = do { putStrLn s ; f }

-- withLocation :: Q Exp
-- withLocation = withFileLine [| withLocation' |]

data ModuleName = ModuleName String

moduleNameTH :: Q Exp
moduleNameTH = do
  loc <- location
  lift (loc_module loc)

withLocation :: Q Exp -> Q Exp
withLocation f = do
    let loc = fileLine =<< location
    appE f loc

fileLine :: Loc -> Q Exp
fileLine loc = do
    let floc = formatLoc loc
    [| $(litE $ stringL floc) |]

formatLoc :: Loc -> String
formatLoc loc = 
  let file = loc_filename loc
      (line, col) = loc_start loc
  in concat [file, ":", show line, ":", show col]
