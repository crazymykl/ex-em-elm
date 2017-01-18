module ExEmElm.Traverse exposing (queryTag, innerText)

import ExEmElm.Types exposing (Node(..))


queryTag : Node -> String -> List Node
queryTag rootNode tagString =
    case rootNode of
        Element tag attrs children ->
            if tag == tagString then
                [ rootNode ]
            else
                List.concatMap (\n -> queryTag n tagString) children

        _ as e ->
            []


innerText : Node -> String
innerText rootNode =
    case rootNode of
        Text text ->
            text

        Element tag attrs children ->
            List.map innerText children |> String.join ""

        _ ->
            ""
