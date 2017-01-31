module ExEmElm.Builder exposing (document, xmlDecl, node, text, comment, cdata, attribute)

{-| Build an XML tree or document

@docs document, xmlDecl, text, node, text, comment, cdata, attribute
-}

import ExEmElm.Types as E


{-| Create an XML document (with optional declaration)
-}
document : Maybe E.XmlDecl -> E.Node -> E.Document
document =
    E.document


{-| Create an XML declaration
-}
xmlDecl : String -> String -> Bool -> E.XmlDecl
xmlDecl =
    E.xmlDecl


{-| Create a new XML node
-}
node : String -> List E.Attribute -> List E.Node -> E.Node
node =
    E.element


{-| Create text
-}
text : String -> E.Node
text =
    E.text


{-| Create a comment
-}
comment : String -> E.Node
comment =
    E.comment


{-| Create a CDATA section
-}
cdata : String -> E.Node
cdata =
    E.cdata


{-| Create an attribute
-}
attribute : String -> String -> E.Attribute
attribute =
    E.attribute
