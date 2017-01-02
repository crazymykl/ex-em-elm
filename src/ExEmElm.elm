module ExEmElm exposing (Document, Node, parse)

{-| This library allows the parsing of XML

@docs Document, Node

@docs parse

-}

import Combine exposing (..)


{-| An XML Document
-}
type alias Document =
    { declaration : Maybe XmlDecl
    , root : Node
    }


{-| A Node in an XML Document
-}
type Node
    = Element String (List Attribute) (List Node)
    | Text String
    | Comment String
    | CDATA String


type alias Attribute =
    { name : String, value : String }


type alias XmlDecl =
    { version : String
    , encoding : String
    , standalone : Bool
    }


{-| Parses a String of XML
-}
parse : String -> Result String Document
parse =
    Combine.parse document
        >> Result.mapError errMsg
        >> Result.map third


third : ( a, b, c ) -> c
third ( _, _, c ) =
    c


errMsg : ParseErr () -> String
errMsg =
    third
        >> List.intersperse "\n"
        >> String.concat


document : Parser s Document
document =
    Document
        <$> maybe xmlDecl
        <*> node
        <* end


defaultXmlDecl : XmlDecl
defaultXmlDecl =
    XmlDecl "1.0" "UTF-8" True


xmlDecl : Parser s XmlDecl
xmlDecl =
    XmlDecl
        <$> (string "<?xml version" *> eq *> attributeValue)
        <*> optional defaultXmlDecl.encoding (string " encoding" *> eq *> attributeValue)
        <*> optional defaultXmlDecl.standalone (string " standalone" *> eq *> quotedYesNo)
        <* (maybe whitespace <* string "?>")


quotedYesNo : Parser s Bool
quotedYesNo =
    let
        yn =
            string "yes" <|> string "no"

        quotedBy x =
            between (string x) (string x)
    in
        (==) "yes" <$> ((quotedBy "'" yn) <|> (quotedBy "\"" yn))


node : Parser s Node
node =
    choice [ cdata, text, comment, tag ]


comment : Parser s Node
comment =
    Comment <$> (string "<!--" *> regex "(?:[^-]|-[^-])*" <* string "-->")


tag : Parser s Node
tag =
    let
        tag_ () =
            Element
                <$> (string "<" *> name)
                <*> (many whitespace *> attributeList)
                <*> (selfCloseTag <|> (string ">" *> many node <* closeTag))
    in
        lazy tag_


name : Parser s String
name =
    regex "[:a-zA-Z_][-:.0-9a-zA-Z_]*"


text : Parser s Node
text =
    Text <$> regex "[^<]+"


cdata : Parser s Node
cdata =
    CDATA <$> (string "<![CDATA[" *> regex ".*(?=]]>)" <* string "]]>")


selfCloseTag : Parser s (List Node)
selfCloseTag =
    string "/>" $> []


closeTag : Parser s String
closeTag =
    between (string "</") (string ">") name


eq : Parser s String
eq =
    maybe whitespace *> string "=" <* maybe whitespace


attribute : Parser s Attribute
attribute =
    Attribute
        <$> name
        <*> (eq *> attributeValue)


attributeValue : Parser s String
attributeValue =
    String.slice 1 -1 <$> regex "'[^']*'|\"[^\"]*\""


attributeList : Parser s (List Attribute)
attributeList =
    sepEndBy whitespace attribute
