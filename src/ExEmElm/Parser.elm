module ExEmElm.Parser exposing (parse, root)

{-| This library allows the parsing of XML

@docs parse, root

-}

import ExEmElm.Types as E exposing (Document, Node, XmlDecl, Attribute)
import Combine exposing (..)


{-| Parses an XML document from a string
-}
parse : String -> Result String Document
parse =
    Combine.parse document
        >> Result.mapError errMsg
        >> Result.map third


{-| Retrives the root node of a document
-}
root : Document -> Node
root =
    E.root


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
    E.document
        <$> maybe xmlDecl
        <*> (many whitespace *> element <* many whitespace)
        <* many whitespace
        <* end


defaultXmlDecl : XmlDecl
defaultXmlDecl =
    E.xmlDecl "1.0" "UTF-8" True


xmlDecl : Parser s XmlDecl
xmlDecl =
    E.xmlDecl
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
    lazy <| \() -> choice [ cdata, text, comment, element ]


comment : Parser s Node
comment =
    E.comment <$> (string "<!--" *> regex "(?:[^-]|-[^-])*" <* string "-->")


element : Parser s Node
element =
    E.element
        <$> (string "<" *> name)
        <*> (many whitespace *> attributeList)
        <*> (selfCloseTag <|> (string ">" *> many node <* closeTag))


name : Parser s String
name =
    regex "[:a-zA-Z_][-:.0-9a-zA-Z_]*"


text : Parser s Node
text =
    E.text <$> regex "[^<]+"


cdata : Parser s Node
cdata =
    E.cdata <$> (string "<![CDATA[" *> regex ".*(?=]]>)" <* string "]]>")


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
    E.attribute
        <$> name
        <*> (eq *> attributeValue)


attributeValue : Parser s String
attributeValue =
    String.slice 1 -1 <$> regex "'[^']*'|\"[^\"]*\""


attributeList : Parser s (List Attribute)
attributeList =
    sepEndBy whitespace attribute
