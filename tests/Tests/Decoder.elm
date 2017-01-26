module Tests.Decoder exposing (decoder)

import Test exposing (..)
import Expect
import Result
import ExEmElm.Types exposing (Node)
import ExEmElm.Decoder exposing (field, int, float, string, bool, list)
import ExEmElm.Traverse exposing (at)
import Tests.Helpers exposing (rootNodeOfXml)


type alias Thing =
    { id : Int, name : String, shine : Float, happy : Bool }


xml : String
xml =
    """
<root>
    <thing>
        <id>1</id>
        <name>mew</name>
        <shine>0.7</shine>
        <happy>true</happy>
    </thing>
    <thing>
        <id>2</id>
        <name>boo</name>
        <shine>3.5</shine>
        <happy>true</happy>
    </thing>
    <thing>
        <id>3</id>
        <name/>
        <happy>false</happy>
        <shine>-3.5</shine>
    </thing>
    <lol>12</lol>
</root>
"""


root : Node
root =
    rootNodeOfXml xml


decodeThing : Node -> Result String Thing
decodeThing node =
    let
        get =
            field node
    in
        Ok Thing
            |> get "id" int
            |> get "name" string
            |> get "shine" float
            |> get "happy" bool


things : List Thing
things =
    [ Thing 1 "mew" 0.7 True
    , Thing 2 "boo" 3.5 True
    , Thing 3 "" -3.5 False
    ]


decoder : Test
decoder =
    describe "It is able to decode fields from a node"
        [ test "It can make a whole list of models" <|
            \() ->
                Ok things |> Expect.equal (list decodeThing <| at [ "thing" ] root)
        ]
