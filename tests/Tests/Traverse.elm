module Tests.Traverse exposing (traverse)

import Test exposing (..)
import Expect
import ExEmElm.Encoder
import ExEmElm.Traverse
import Tests.Helpers exposing (rootNodeOfXml)


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
        ExEmElm.Traverse.at qFields rootNode
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


resultQueryByTag1 : String
resultQueryByTag1 =
    """<UIData> <language_options> <Label> <id>63</id> <name>English</name> </Label> </language_options> <filters> <Label> <id>100947</id> <name>5 Store Report</name> </Label> <Label> <id>100946</id> <name>AAA New filter Montevideo</name> </Label> <Label> <id>90158</id> <name>Current VMM Stores- HS</name> </Label> <Label> <id>90157</id> <name>Current live stores</name> </Label> <Label> <id>90187</id> <name>Launch stores</name> </Label> <Label> <id>90159</id> <name>None</name> </Label> <Label> <id>87975</id> <name>Washington State Stores</name> </Label> </filters> </UIData>"""


resultQueryByTag2 : String
resultQueryByTag2 =
    """<language_options> <Label> <id>63</id> <name>English</name> </Label> </language_options>"""


resultQueryByTag3 : String
resultQueryByTag3 =
    """<Label> <id>63</id> <name>English</name> </Label>"""


resultQueryByTag4 : String
resultQueryByTag4 =
    "<id>63</id>"


resultQueryByTag5 : String
resultQueryByTag5 =
    """<filters> <Label> <id>100947</id> <name>5 Store Report</name> </Label> <Label> <id>100946</id> <name>AAA New filter Montevideo</name> </Label> <Label> <id>90158</id> <name>Current VMM Stores- HS</name> </Label> <Label> <id>90157</id> <name>Current live stores</name> </Label> <Label> <id>90187</id> <name>Launch stores</name> </Label> <Label> <id>90159</id> <name>None</name> </Label> <Label> <id>87975</id> <name>Washington State Stores</name> </Label> </filters>"""


resultQueryByTag6 : String
resultQueryByTag6 =
    """<Label> <id>100947</id> <name>5 Store Report</name> </Label><Label> <id>100946</id> <name>AAA New filter Montevideo</name> </Label><Label> <id>90158</id> <name>Current VMM Stores- HS</name> </Label><Label> <id>90157</id> <name>Current live stores</name> </Label><Label> <id>90187</id> <name>Launch stores</name> </Label><Label> <id>90159</id> <name>None</name> </Label><Label> <id>87975</id> <name>Washington State Stores</name> </Label>"""


resultQueryByTag7 : String
resultQueryByTag7 =
    """<id>100947</id><id>100946</id><id>90158</id><id>90157</id><id>90187</id><id>90159</id><id>87975</id>"""


resultQueryByTag8 : String
resultQueryByTag8 =
    """<name>5 Store Report</name><name>AAA New filter Montevideo</name><name>Current VMM Stores- HS</name><name>Current live stores</name><name>Launch stores</name><name>None</name><name>Washington State Stores</name>"""


traverse : Test
traverse =
    describe "It queries the xml tree"
        [ test "It traverses from root" <| assertTreeEncodes testXmlString resultXmlString
        , test "It stringifies inner XML" <| assertInnerTextTraversal testXmlString resultInnerXmlString
        , test "queries for nested Node according to an empty path" <| assertQueryByTag testXmlString [] resultQueryByTag1
        , test "queries for nested Node according to path: language_options" <| assertQueryByTag testXmlString [ "language_options" ] resultQueryByTag2
        , test "queries for nested Node according to path: language_options, Label" <| assertQueryByTag testXmlString [ "language_options", "Label" ] resultQueryByTag3
        , test "queries for nested Node according to path: language_options, Label, id" <| assertQueryByTag testXmlString [ "language_options", "Label", "id" ] resultQueryByTag4
        , test "queries for nested Node according to path: filters" <| assertQueryByTag testXmlString [ "filters" ] resultQueryByTag5
        , test "queries for nested Node according to path: filters, Label" <| assertQueryByTag testXmlString [ "filters", "Label" ] resultQueryByTag6
        , test "queries for nested Node according to path: filters, Label, id" <| assertQueryByTag testXmlString [ "filters", "Label", "id" ] resultQueryByTag7
        , test "queries for nested Node according to path: filters, Label, name" <| assertQueryByTag testXmlString [ "filters", "Label", "name" ] resultQueryByTag8
        ]
