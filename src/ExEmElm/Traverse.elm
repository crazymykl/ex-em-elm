module ExEmElm.Traverse exposing (queryTag, innerText)

import ExEmElm.Types exposing (Node, childrenOfNode, tagOfNode, textOfNode)


queryTag : Node -> String -> List Node
queryTag rootNode tagString =
    let
        tag =
            tagOfNode rootNode

        children =
            childrenOfNode rootNode
    in
        if tag == tagString then
            [ rootNode ]
        else
            List.concatMap (\n -> queryTag n tagString) children


innerText : Node -> String
innerText rootNode =
    let
        text =
            textOfNode rootNode

        children =
            childrenOfNode rootNode
    in
        if text /= "" then
            text
        else
            List.map innerText children |> String.join ""
