module Tests exposing (..)

import Test exposing (..)
import Expect
import Result
import ExEmElm


isOk : Result e a -> Bool
isOk x =
    case x of
        Ok _ ->
            True

        Err _ ->
            False


assertParses : String -> () -> Expect.Expectation
assertParses s () =
    ExEmElm.parse s
        |> isOk
        |> Expect.true ("Expected '" ++ s ++ "' to parse successfully")


refuteParses : String -> () -> Expect.Expectation
refuteParses s () =
    ExEmElm.parse s
        |> isOk
        |> Expect.false ("Expected '" ++ s ++ "' not to parse")


all : Test
all =
    describe "Parsing Smoke Test"
        [ test "It parses emptiness" <| assertParses ""
        , test "It parses text" <| assertParses "foo"
        , test "It parses self-closing tags" <| assertParses "<mew/>"
        , test "It parses tags containing text" <| assertParses "<arf>1</arf>"
        , test "It parses nested tags" <| assertParses "<poo><goo/></poo>"
        , test "It parses tags with attributes" <| assertParses "<foo mew='lol'/>"
        , test "It barfs on malformed tags" <| refuteParses "<foo "
        , test "It barfs on invalid tag names" <| refuteParses "<21></21>"
        , test "It barfs on unclosed tags" <| refuteParses "<mew>"
        , test "It barfs on illegal attributes" <| refuteParses "<d 1='2'/>"
        ]
