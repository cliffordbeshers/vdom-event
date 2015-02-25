module GHCJSStub.DOM where

class IsDOMWindow a where
data Document = Document

webViewGetDomDocument :: IsDOMWindow w => w -> IO (Maybe Document)
webViewGetDomDocument = domWindowGetDocument
  where domWindowGetDocument = undefined

runWebGUI :: Int
runWebGUI = undefined
