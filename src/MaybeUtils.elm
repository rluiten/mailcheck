module MaybeUtils exposing
    ( thenOneOf
    , firstOneOf
    )

{-| Utilities I have been finding useful to work with Maybe.

@docs thenOneOf
@docs firstOneOf

(c) 2015 Robin Luiten

-}

import Maybe


{-| Get first Just result from a list of functions returning Maybe.
-}
thenOneOf : List (a -> Maybe b) -> Maybe a -> Maybe b
thenOneOf listFuncs maybeVal =
    maybeVal |> Maybe.andThen (firstOneOf listFuncs)


{-| Created for thenOneof function, but useful itself.

Return the first Just value applying each of funtions sequentially.

-}
firstOneOf : List (a -> Maybe b) -> a -> Maybe b
firstOneOf funcs val =
    case funcs of
        func :: restFuncs ->
            case func val of
                Just output ->
                    Just output

                Nothing ->
                    firstOneOf restFuncs val

        [] ->
            Nothing
