module AppraisalScribe.FakeJMacro where

data JStat = JStat deriving (Eq, Ord, Typeable, Data, Show)
data JExpr = JExpr deriving (Eq, Ord, Typeable, Data, Show)
data Doc = Doc deriving (Show)
class JMacro a where
  jmacro :: a -> a

class JsToDoc a where
  renderJs :: a -> Doc
class ToJExpr a where
  toJExpr :: a -> JExpr


