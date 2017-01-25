module ExEmElm.Traverse exposing (innerText, at, textAt)

{-| This library allows the traversal/querying of parsed XML

@docs innerText, at, textAt

-}

import ExEmElm.Types exposing (Node, childrenOfNode, tagOfNode, textOfNode)


{-| Retrives the text inside the node and all subnodes
-}
innerText : Node -> String
innerText node =
    case textOfNode node of
        Just text ->
            text

        Nothing ->
            List.map innerText (childrenOfNode node)
                |> String.join ""


{-| Traverse down a path of tags
-}
at : List String -> Node -> List Node
at path node =
    let
        follow path node =
            if List.isEmpty path then
                [ node ]
            else
                List.concatMap (matchesPath path) <| childrenOfNode node

        matchesPath path node =
            case path of
                [] ->
                    []

                x :: xs ->
                    if Just x == tagOfNode node then
                        follow xs node
                    else
                        []
    in
        follow path node


{-| Get the text inside all tags at the end of the given path
-}
textAt : List String -> Node -> List String
textAt path node =
    at path node
        |> List.map innerText
