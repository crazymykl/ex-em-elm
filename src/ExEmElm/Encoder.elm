module ExEmElm.Encoder exposing (document, node)

{-| This library stringifies XML data structures

@docs document, node
-}

import ExEmElm.Types as E exposing (Document, Node, XmlDecl, Attribute)


{-| Stringify an entire XML document
-}
document : Document -> String
document =
    E.docToString


{-| Stringify an individual XML node
-}
node : Node -> String
node =
    E.nodeToString
