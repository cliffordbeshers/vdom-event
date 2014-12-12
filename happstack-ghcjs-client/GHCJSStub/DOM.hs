module GHCJSStub.DOM where

webViewGetDomDocument :: IsDOMWindow w => w -> IO (Maybe Document)
webViewGetDomDocument = domWindowGetDocument
  where domWindowGetDocument = undefined

runWebGUI :: Int
runWebGUI = undefined
