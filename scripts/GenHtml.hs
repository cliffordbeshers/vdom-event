import System.FilePath as FilePath (FilePath, (</>))
import Data.Monoid ((<>))
import Data.List as List (intercalate, words)

main = do
  writeFile (htmlDir </> "Elements.hs") $ catln elementCode
  writeFile (htmlDir </> "Attributes.hs") $ catln attributeCode

htmlDir :: FilePath
htmlDir = "Alderon/Html"

moduleDir :: FilePath -> String
moduleDir = fmap (replace '/' '.')
  where replace p r x = if p == x then r else x

html_tags :: [String]
html_tags = concat $ map words
            [ "a abbr address area article aside audio b base bdi bdo big blockquote body br"
            , "button canvas caption cite code col colgroup data datalist dd del details"
            , "dfn div dl dt em embed fieldset figcaption figure footer form"
            , "h1 h2 h3 h4 h5 h6"
            , "head header hr html i iframe img input ins kbd keygen label legend li link"
            , "main map mark menu menuitem meta meter nav noscript object ol optgroup option"
            , "output p param pre progress q rp rt ruby s samp script section select small"
            , "source span strong style sub summary sup table tbody td textarea tfoot th"
            , "thead time title tr track u ul var video wbr"
            ]

void_tags :: [String]
void_tags = concat $ map words
  [ "area base br col command embed hr img input"
  , "keygen link meta param source track wbr"
  ]

attr1_tags :: [String]
attr1_tags = concat $ map words
  [ "accept accessKey action allowFullScreen allowTransparency alt cellPadding"
  , "cellSpacing charset checked colSpan cols content contentEditable contextMenu"
  , "controls data dateTime dir draggable encType for form formNoValidate"
  , "frameBorder height href htmlFor icon label lang list max maxLength method"
  , "min name pattern placeholder poster radioGroup readOnly rel role rowSpan rows"
  , "sandbox scope scrollLeft scrollTop selected size span spellCheck src srcDoc"
  , "step style tabIndex target title type value width wmode"
  ]

attr0_tags :: [String]
attr0_tags = concat $ map words
  [ "async autocomplete autofocus autoplay checked defer disabled download hidden"
  , "loop multiple noValidate preload readOnly required reversed seamless"
  , "spellCheck"
  ]


elementCode :: [String]
elementCode = header <> (concat $ map arity1 html_tags <> map arity0 void_tags)
  where header = map cats
                 [ ["module", moduleDir (htmlDir </> "Elements"), "where"]
                 , ["import", moduleDir (htmlDir </> "Internal")]
                 ]
        arity0 = eltcode "leaf"
        arity1 = eltcode "parent"
        eltcode :: String -> String -> [String]
        eltcode comb e = let e' = e <> "_" in
          map cats
          [ [e', hasType, htmlT, funarr, htmlT]
          , [e', eqsign, comb, show e]
          , []
          ]

attributeCode = header <> concat (map attr1code attr1_tags <> map attr0code attr0_tags)
  where header = map cats
                 [ ["module", moduleDir (htmlDir </> "Attributes"), "(", ")", "where"]
                 , ["import", moduleDir (htmlDir </> "Internal")]
                 ]
        attr1code = attrcode "attribute"
        attr0code = attrcode "boolean"
        attrcode comb e =  let e' = e <> "_" in
          map cats
          [ [e', hasType, textT, funarr, attributeT]
          , [e', eqsign, comb, show e]
          , []
          ]

eqsign, hasType, funarr, htmlT, textT, attributeT :: String
eqsign = "="
hasType = "::"
funarr = "->"
htmlT = "Html"
textT = "Text"
attributeT = "Attribute"

cats = intercalate " "
catln = intercalate "\n"
