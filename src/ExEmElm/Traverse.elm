module ExEmElm.Traverse exposing (innerText, at)

import ExEmElm.Types exposing (Node, childrenOfNode, tagOfNode, textOfNode)


innerText : Node -> String
innerText rootNode =
    let
        text =
            textOfNode rootNode
                |> Maybe.withDefault ""

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
                if Just x == tag then
                    [ rootNode ]
                else
                    []

            x :: xs ->
                if Just x == tag then
                    List.concatMap (flip at xs) children
                else
                    []
