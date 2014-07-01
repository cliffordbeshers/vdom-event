#!/usr/bin/runhaskell 
module Main where
import Distribution.Simple (defaultMainWithHooks, simpleUserHooks, UserHooks(postBuild))
import Distribution.Simple.LocalBuildInfo (buildDir)
import System.FilePath ((</>), (<.>))
import System.IO (appendFile)
import System.Process (readProcess)

packageName :: FilePath
packageName = "happstack-ghcjs-client"

main :: IO ()
main = defaultMainWithHooks $ simpleUserHooks
         { postBuild = \ _ _ _ lbi -> let prefix = buildDir lbi </> packageName </> packageName <.> "jsexe" 
                                          binpath = "usr/bin" </> packageName <.> "jsexe"
                                      in
                                        readProcess "find" [prefix, "-type", "f"] "" >>=
                                        writeFile ("debian" </> packageName <.> "install") . unlines . map (++ " " ++ binpath) . lines }

