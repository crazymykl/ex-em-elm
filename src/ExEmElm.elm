module ExEmElm exposing (Document, Node, parse)

{-| This library allows the parsing of XML

@docs Document, Node

@docs parse

-}

import Combine exposing (..)


{-| An XML Document
-}
type Document
    = Document (List Node)


{-| A Node in an XML Document
-}
type Node
    = Element String (List Attribute) (List Node)
    | Text String


type alias Attribute =
    { name : String, value : String }


{-| Parses a String of XML
-}
parse : String -> Result (ParseErr ()) (ParseOk () Document)
parse =
    Combine.parse document


document : Parser s Document
document =
    Document <$> manyTill node end


node : Parser s Node
node =
    text <|> tag


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


selfCloseTag : Parser s (List Node)
selfCloseTag =
    string "/>" $> []


closeTag : Parser s String
closeTag =
    between (string "</") (string ">") name


attribute : Parser s Attribute
attribute =
    Attribute
        <$> name
        <*> (string "=" *> attributeValue)


attributeValue : Parser s String
attributeValue =
    regex "'[^']*'|\"[^\"]*\""


attributeList : Parser s (List Attribute)
attributeList =
    sepEndBy whitespace attribute
