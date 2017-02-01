module ExEmElm.Decoder exposing (string, int, float, bool, field, list, maybe, Node)

{-| Access and decode fields from XML elements

@docs Node, field

@docs maybe, list, string, int, float, bool
-}

import ExEmElm.Traverse exposing (textAt)
import ExEmElm.Types


{-| Represents an XML node
-}
type alias Node =
    ExEmElm.Types.Node


{-| Decode a string
-}
string : List a -> Result String a
string =
    Result.fromMaybe "Value not found" << List.head


{-| Decode an integer
-}
int : List String -> Result String Int
int =
    Result.andThen String.toInt << string


{-| Decode a float
-}
float : List String -> Result String Float
float =
    Result.andThen String.toFloat << string


{-| Decode a boolean
-}
bool : List String -> Result String Bool
bool x =
    case string x of
        Ok "true" ->
            Ok True

        Ok "false" ->
            Ok False

        Ok val ->
            Err (val ++ " is not a boolean")

        Err err ->
            Err err


combine : List (Result e a) -> Result e (List a)
combine =
    List.foldr (Result.map2 (::)) (Ok [])


{-| Decode a list of the same structures
-}
list : (a -> Result e b) -> List a -> Result e (List b)
list f xs =
    List.map f xs
        |> combine


{-| Make another decoder optional
-}
maybe : (List a -> Result String b) -> List a -> Result String (Maybe b)
maybe f xs =
    f xs
        |> Result.toMaybe
        |> Ok


{-| Access the data contained in the children with a given tag, then decode it

Note that most of the decoders discard all but the first matching value
-}
field : Node -> String -> (List String -> Result e a) -> Result e (a -> b) -> Result e b
field node name type_ =
    textAt [ name ] node
        |> type_
        |> Result.map2 (|>)
