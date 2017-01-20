module Tests exposing (..)

import Test exposing (..)
import Expect
import Result
import Result.Extra
import ExEmElm.Parser
import ExEmElm.Encoder
import ExEmElm.Types exposing (root, element, text, comment, cdata, attribute)
import ExEmElm.Traverse exposing (innerText, at)


parses : String -> Bool
parses =
    ExEmElm.Parser.parse >> Result.Extra.isOk


rootNodeOfXml : String -> ExEmElm.Types.Node
rootNodeOfXml xmlString =
    ExEmElm.Parser.parse xmlString
        |> Result.map root
        |> Result.withDefault (text "Error")


assertParses : String -> () -> Expect.Expectation
assertParses s () =
    parses s
        |> Expect.true ("Expected '" ++ s ++ "' to parse successfully")


refuteParses : String -> () -> Expect.Expectation
refuteParses s () =
    parses s
        |> Expect.false ("Expected '" ++ s ++ "' not to parse")


assertNodeEncodesTo : ExEmElm.Types.Node -> String -> () -> Expect.Expectation
assertNodeEncodesTo node str () =
    Expect.equal (ExEmElm.Encoder.node node) str


assertNodeRoundtrips : String -> () -> Expect.Expectation
assertNodeRoundtrips str =
    let
        parsedNode =
            ExEmElm.Parser.parse str
                |> Result.map root
                |> Result.withDefault (text "Unparsable")
    in
        assertNodeEncodesTo parsedNode str


assertTreeEncodes : String -> String -> () -> Expect.Expectation
assertTreeEncodes xmlString resultXmlString () =
    let
        rootNode =
            rootNodeOfXml xmlString
    in
        ExEmElm.Encoder.node rootNode
            |> Expect.equal resultXmlString


assertQueryByTag : String -> List String -> String -> () -> Expect.Expectation
assertQueryByTag xmlString qFields resultXmlString () =
    let
        rootNode =
            rootNodeOfXml xmlString
    in
        ExEmElm.Traverse.at rootNode qFields
            |> List.map ExEmElm.Encoder.node
            |> String.join ""
            |> Expect.equal resultXmlString


assertInnerTextTraversal : String -> String -> () -> Expect.Expectation
assertInnerTextTraversal xmlString resultInnerXmlString () =
    let
        rootNode =
            rootNodeOfXml xmlString
    in
        ExEmElm.Traverse.innerText rootNode
            |> Expect.equal resultInnerXmlString


all : Test
all =
    concat [ smoke, encode, traverse ]


smoke : Test
smoke =
    describe "Parsing Smoke Test"
        [ test "It parses self-closing tags" <| assertParses "<mew/>"
        , test "It parses tags containing text" <| assertParses "<arf>1</arf>"
        , test "It parses nested tags" <| assertParses "<poo><goo/></poo>"
        , test "It parses tags with attributes" <| assertParses "<foo mew='lol'/>"
        , test "It parses comments" <| assertParses "<e><!-- waffle iron --></e>"
        , test "It parses CDATA" <| assertParses "<e><![CDATA[ ]]></e>"
        , test "It parses nested tags" <| assertParses "<foo><bar/></foo>"
        , test "It barfs on emptiness" <| refuteParses ""
        , test "It barfs on naked text" <| refuteParses "lollygag"
        , test "It barfs on naked comments" <| refuteParses "<!-- hihi -->"
        , test "It barfs on naked CDATA" <| refuteParses "<![CDATA[ yo ]]>"
        , test "It barfs on malformed tags" <| refuteParses "<foo "
        , test "It barfs on invalid tag names" <| refuteParses "<21></21>"
        , test "It barfs on unclosed tags" <| refuteParses "<mew>"
        , test "It barfs on illegal attributes" <| refuteParses "<d 1='2'/>"
        ]


encode : Test
encode =
    describe "It stringifies XML"
        [ test "It encodes text" <| assertNodeEncodesTo (text "foo") "foo"
        , test "It encodes comments" <| assertNodeEncodesTo (comment "bar") "<!--bar-->"
        , test "It encodes CDATA" <| assertNodeEncodesTo (cdata "baz") "<![CDATA[baz]]>"
        , test "It encodes empty tags" <| assertNodeEncodesTo (element "quux" [] []) "<quux></quux>"
        , test "It encodes tags with children" <|
            assertNodeEncodesTo
                (element "quux" [] [ text "yo" ])
                "<quux>yo</quux>"
        , test "It encodes tags with attibutes" <|
            assertNodeEncodesTo
                (element "quux" [ attribute "foo" "bar" ] [])
                "<quux foo=\"bar\"></quux>"
        , test "It roundtrips canonical XML" <|
            assertNodeRoundtrips
                """<smurf man="poo"> <!-- mew --> lol <o><![CDATA[ goo ]]></o></smurf>"""
        ]


testXmlString : String
testXmlString =
    """<?xml version="1.0" encoding="UTF-8"?> <UIData> <language_options> <Label> <id>63</id> <name>English</name> </Label> </language_options> <filters> <Label> <id>100947</id> <name>5 Store Report</name> </Label> <Label> <id>100946</id> <name>AAA New filter Montevideo</name> </Label> <Label> <id>90158</id> <name>Current VMM Stores- HS</name> </Label> <Label> <id>90157</id> <name>Current live stores</name> </Label> <Label> <id>90187</id> <name>Launch stores</name> </Label> <Label> <id>90159</id> <name>None</name> </Label> <Label> <id>87975</id> <name>Washington State Stores</name> </Label> </filters> </UIData>"""


resultXmlString : String
resultXmlString =
    """<UIData> <language_options> <Label> <id>63</id> <name>English</name> </Label> </language_options> <filters> <Label> <id>100947</id> <name>5 Store Report</name> </Label> <Label> <id>100946</id> <name>AAA New filter Montevideo</name> </Label> <Label> <id>90158</id> <name>Current VMM Stores- HS</name> </Label> <Label> <id>90157</id> <name>Current live stores</name> </Label> <Label> <id>90187</id> <name>Launch stores</name> </Label> <Label> <id>90159</id> <name>None</name> </Label> <Label> <id>87975</id> <name>Washington State Stores</name> </Label> </filters> </UIData>"""


languageOptionsString : String
languageOptionsString =
    """<language_options> <Label> <id>63</id> <name>English</name> </Label> </language_options>"""


resultInnerXmlString : String
resultInnerXmlString =
    """   63 English     100947 5 Store Report   100946 AAA New filter Montevideo   90158 Current VMM Stores- HS   90157 Current live stores   90187 Launch stores   90159 None   87975 Washington State Stores   """


resultQueryByTag : String
resultQueryByTag =
    "<id>63</id>"


traverse : Test
traverse =
    describe "It queries the xml tree"
        [ test "It traverses from root" <| assertTreeEncodes testXmlString resultXmlString
        , test "It stringifies inner XML" <| assertInnerTextTraversal testXmlString resultInnerXmlString
        , test "queries for nested Node according to path: UIData, language_options, Label, id" <| assertQueryByTag testXmlString [ "UIData", "language_options", "Label", "id" ] resultQueryByTag
        ]
