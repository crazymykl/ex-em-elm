module Tests exposing (..)

import Test exposing (..)
import Expect
import Result
import Result.Extra
import ExEmElm.Parser
import ExEmElm.Encoder
import ExEmElm.Types exposing (root, element, text, comment, cdata, attribute)
import ExEmElm.Traverse exposing (innerText, at)
import Tests.Traverse exposing (traverse)
import Tests.Decoder exposing (decoder)
import Tests.Helpers exposing (rootNodeOfXml)


parses : String -> Bool
parses =
    ExEmElm.Parser.parse >> Result.Extra.isOk


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
    assertNodeEncodesTo (rootNodeOfXml str) str


all : Test
all =
    concat [ smoke, encode, traverse, decoder ]


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
