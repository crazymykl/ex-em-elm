module ExEmElm.Traverse exposing (innerText, at)

import ExEmElm.Types exposing (Node, childrenOfNode, tagOfNode, textOfNode)


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


at : Node -> List String -> List Node
at rootNode fields =
    let
        tag =
            tagOfNode rootNode

        children =
            childrenOfNode rootNode
    in
        case fields of
            [] ->
                []

            [ x ] ->
                if x == tag then
                    [ rootNode ]
                else
                    []

            x :: xs ->
                if x == tag then
                    List.concatMap (\n -> at n xs) children
                else
                    []
