module ExEmElm.Types
    exposing
        ( Document
        , Node
        , Attribute
        , XmlDecl
        , document
        , root
        , element
        , text
        , comment
        , cdata
        , attribute
        , xmlDecl
        , nodeToString
        , docToString
        )

import Regex exposing (regex, replace, HowMany(..))


type Document
    = Document
        { declaration : Maybe XmlDecl
        , root : Node
        }


type Node
    = Element String (List Attribute) (List Node)
    | Text String
    | Comment String
    | CDATA String


type Attribute
    = Attribute { name : String, value : String }


type alias XmlDecl =
    { version : String
    , encoding : String
    , standalone : Bool
    }


document : Maybe XmlDecl -> Node -> Document
document decl root =
    Document
        { declaration = decl
        , root = root
        }


root : Document -> Node
root (Document doc) =
    doc.root


element : String -> List Attribute -> List Node -> Node
element =
    Element


text : String -> Node
text =
    Text


comment : String -> Node
comment =
    Comment


cdata : String -> Node
cdata =
    CDATA


attribute : String -> String -> Attribute
attribute name value =
    Attribute
        { name = name
        , value = value
        }


xmlDecl : String -> String -> Bool -> XmlDecl
xmlDecl =
    XmlDecl


attrToString : Attribute -> String
attrToString (Attribute attribute) =
    attribute.name ++ "=\"" ++ (encodeEntities attribute.value) ++ "\""


attrsToString : List Attribute -> String
attrsToString attributeList =
    ""
        :: List.map attrToString attributeList
        |> String.join " "


encodeEntities : String -> String
encodeEntities =
    replace All (regex "\"") (always "&quot;")


nodeToString : Node -> String
nodeToString x =
    case x of
        Element tag attrs children ->
            let
                cookedAttrs =
                    attrsToString attrs

                cookedChildren =
                    List.map nodeToString children |> String.join ""
            in
                "<" ++ tag ++ cookedAttrs ++ ">" ++ cookedChildren ++ "</" ++ tag ++ ">"

        Text text ->
            text

        Comment comment ->
            "<!--" ++ comment ++ "-->"

        CDATA cdata ->
            "<![CDATA[" ++ cdata ++ "]]>"


docToString : Document -> String
docToString (Document x) =
    decl x.declaration ++ nodeToString x.root


decl : Maybe XmlDecl -> String
decl =
    let
        yesno b =
            if b then
                "yes"
            else
                "no"

        declTag c =
            "<?xml"
                ++ (" version=\"" ++ c.version ++ "\"")
                ++ (" encoding=\"" ++ c.encoding ++ "\"")
                ++ (" standalone=\"" ++ yesno c.standalone ++ "\"")
                ++ " ?>"
    in
        Maybe.map declTag >> Maybe.withDefault ""
