module Tests.Helpers exposing (rootNodeOfXml)

import Result
import ExEmElm.Parser
import ExEmElm.Types exposing (root, text, Node)


rootNodeOfXml : String -> Node
rootNodeOfXml xmlString =
    ExEmElm.Parser.parse xmlString
        |> Result.map root
        |> Result.withDefault (text "Error")
