module MaybeUtils exposing (..)

{-| Utilities i have been finding useful to work with Maybe.

`thenOneOf`
* in the vein of https://github.com/NoRedInk/elm-rails/wiki/NoRedInk's-Elm-Style-Guide#use-flip-andor-pipes-over-backticks
* I have been using `thenAnd` as well.

xdocs thenAnd
@docs thenOneOf
@docs firstOneOf

-}
import Maybe

{-| Get first Just result from a list of functions returning Maybe.
-}
thenOneOf : List ((a -> Maybe b)) -> Maybe a -> Maybe b
thenOneOf listFuncs maybeVal =
     maybeVal |> Maybe.andThen (firstOneOf listFuncs)

    -- equivalent not using andThen
    -- case maybeVal of
    --   Just val -> firstOneOf listFuncs val
    --   Nothing -> Nothing


{-| Created for thenOneof function, but useful itself. -}
firstOneOf : List ((a -> Maybe b)) -> a -> Maybe b
firstOneOf funcs val =
    case funcs of
      func :: restFuncs ->
        case func val of
          Just output -> Just output
          Nothing -> firstOneOf restFuncs val
      [] ->
        Nothing
