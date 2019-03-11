module Mailcheck exposing
    ( suggest
    , suggestWith
    , encodeEmail
    , mailParts
    , MailParts
    , findClosestDomain
    , defaultDomains
    , defaultTopLevelDomains
    , defaultSecondLevelDomains
    )

{-| A library that suggests a correct domain when a user miss spells an email address.
This is a port of this javascript library <https://github.com/mailcheck/mailcheck>


## Basic Usage

        Mailcheck.suggest 'test@gnail.com'
          == Just ("test", "gmail.com", "test@gmail.com")

        Mailcheck.suggest 'test@gsnail.com'
          == Nothing


# Create

@docs suggest
@docs suggestWith


# Utility

@docs encodeEmail
@docs mailParts
@docs MailParts
@docs findClosestDomain


# Default domain lists used by suggest

@docs defaultDomains
@docs defaultTopLevelDomains
@docs defaultSecondLevelDomains

(c) 2015 Robin Luiten

-}

import Dict
import Maybe
import MaybeUtils exposing (thenOneOf)
import Regex
import String exposing (String)
import StringDistance exposing (sift3Distance)
import Tuple exposing (second)
import Url exposing (percentEncode)


{-| Record type alias for mailparts.
-}
type alias MailParts =
    { topLevelDomain : String
    , secondLevelDomain : String
    , domain : String
    , address : String
    }


domainThreshold =
    2


secondLevelThreshold =
    2


topLevelThreshold =
    2


{-| Suggest a domain which may assist a user with a possible error
in a candidate email address. This version uses the default internal lists
of domains.

        suggest 'test@gmail.co'

is equivalent to

        suggestWith defaultDomains defaultSecondLevelDomains defaultTopLevelDomains 'test@gmail.co'

example

        (suggest "user@gmil.com")
          == Just ("user", "gmail.com", "user@gmail.com")

Result is Maybe (address, domain, secondLevelDomain, topLevelDomain)

-}
suggest : String -> Maybe ( String, String, String )
suggest =
    suggestWith
        defaultDomains
        defaultSecondLevelDomains
        defaultTopLevelDomains


{-| Suggest with passed in domain lists.

  - domains is list of known valid domains
  - top level domains is allowed to be empty
  - second level domains is allowed to be empty

-}
suggestWith :
    List String
    -> List String
    -> List String
    -> String
    -> Maybe ( String, String, String )
suggestWith domains secondLevelDomains topLevelDomains email =
    mailParts (String.toLower email)
        |> Maybe.andThen (checkPartsNotInList secondLevelDomains topLevelDomains)
        |> thenOneOf
            [ closestDomain domains
            , closestSecondLevelDomain secondLevelDomains topLevelDomains
            ]


{-| Check that secondLevelDomain and topLevelDomain are not in the accepted list of domains
-}
checkPartsNotInList : List String -> List String -> MailParts -> Maybe MailParts
checkPartsNotInList secondLevelDomains topLevelDomains mailPartsX =
    if not (List.member mailPartsX.secondLevelDomain secondLevelDomains && List.member mailPartsX.topLevelDomain topLevelDomains) then
        Just mailPartsX

    else
        Nothing


closestDomain : List String -> MailParts -> Maybe ( String, String, String )
closestDomain domains mailPartsX =
    let
        closestDomainHelper =
            findClosestDomain mailPartsX.domain domains

        result closestDomainVal =
            Just ( mailPartsX.address, closestDomainVal, mailPartsX.address ++ "@" ++ closestDomainVal )

        isDifferentDomain closestDomainVal =
            if closestDomainVal == mailPartsX.domain then
                Nothing

            else
                Just closestDomainVal
    in
    closestDomainHelper
        |> Maybe.andThen isDifferentDomain
        |> Maybe.andThen result


closestSecondLevelDomain : List String -> List String -> MailParts -> Maybe ( String, String, String )
closestSecondLevelDomain secondLevelDomains topLevelDomains mailPartsX =
    let
        findClosest threshold domains =
            findClosestDomain domains

        findResultSld =
            findClosest secondLevelThreshold mailPartsX.secondLevelDomain secondLevelDomains

        findResultTld =
            findClosest topLevelThreshold mailPartsX.topLevelDomain topLevelDomains

        suggestedDomain =
            case ( findResultSld, findResultTld ) of
                ( Nothing, Nothing ) ->
                    mailPartsX.domain

                ( Just closestSld, Nothing ) ->
                    closestSld ++ "." ++ mailPartsX.topLevelDomain

                ( Nothing, Just closestTld ) ->
                    mailPartsX.secondLevelDomain ++ "." ++ closestTld

                ( Just closestSld, Just closestTld ) ->
                    closestSld ++ "." ++ closestTld
    in
    if suggestedDomain == mailPartsX.domain then
        Nothing

    else
        Just
            ( mailPartsX.address
            , suggestedDomain
            , mailPartsX.address ++ "@" ++ suggestedDomain
            )


{-| Split an email address up into components.

        (mailParts "user") == Nothing
        (mailParts "user@") == Nothing
        (mailParts "user@moo.com") ==
          Just
          ( { topLevelDomain = "user"
            , secondLevelDomain = "moo.com"
            , domain = "moo"
            , address = "com"
            }
          )
        (mailParts "user@moo.co.uk") ==
          Just
          ( { topLevelDomain = "user"
            , secondLevelDomain = "moo.com.uk"
            , domain = "moo"
            , address = "co.uk"
            }
          )

-}
mailParts : String -> Maybe MailParts
mailParts mail =
    let
        mailSplit =
            String.trim mail
                |> String.split "@"
                |> List.reverse

        domain =
            List.head mailSplit
                |> Maybe.withDefault ""

        address =
            List.tail mailSplit
                |> Maybe.withDefault []
                |> List.reverse
                |> String.join "@"
    in
    if List.length mailSplit < 2 || List.member "" mailSplit then
        Nothing

    else
        Just
            { topLevelDomain = topLevelDomain domain
            , secondLevelDomain = secondLevelDomain domain
            , domain = domain
            , address = address
            }


topLevelDomain : String -> String
topLevelDomain domain =
    case String.split "." domain of
        x :: [] ->
            x

        _ :: xs ->
            String.join "." xs

        _ ->
            ""


secondLevelDomain : String -> String
secondLevelDomain domain =
    case String.split "." domain of
        x :: [] ->
            ""

        x :: _ ->
            x

        _ ->
            ""


{-| default list of domains used in suggest
-}
defaultDomains : List String
defaultDomains =
    [ "msn.com"
    , "bellsouth.net"
    , "telus.net"
    , "comcast.net"
    , "optusnet.com.au"
    , "earthlink.net"
    , "qq.com"
    , "sky.com"
    , "icloud.com"
    , "mac.com"
    , "sympatico.ca"
    , "googlemail.com"
    , "att.net"
    , "xtra.co.nz"
    , "web.de"
    , "cox.net"
    , "gmail.com"
    , "ymail.com"
    , "aim.com"
    , "rogers.com"
    , "verizon.net"
    , "rocketmail.com"
    , "google.com"
    , "optonline.net"
    , "sbcglobal.net"
    , "aol.com"
    , "me.com"
    , "btinternet.com"
    , "charter.net"
    , "shaw.ca"
    ]


{-| default list of second level domains used in suggest
-}
defaultSecondLevelDomains : List String
defaultSecondLevelDomains =
    [ "yahoo"
    , "hotmail"
    , "mail"
    , "live"
    , "outlook"
    , "gmx"
    ]


{-| default list of top level domains used in suggest
-}
defaultTopLevelDomains : List String
defaultTopLevelDomains =
    [ "com"
    , "com.au"
    , "com.tw"
    , "ca"
    , "co.nz"
    , "co.uk"
    , "de"
    , "fr"
    , "it"
    , "ru"
    , "net"
    , "org"
    , "edu"
    , "gov"
    , "jp"
    , "nl"
    , "kr"
    , "se"
    , "eu"
    , "ie"
    , "co.il"
    , "us"
    , "at"
    , "be"
    , "dk"
    , "hk"
    , "es"
    , "gr"
    , "ch"
    , "no"
    , "cz"
    , "in"
    , "net"
    , "net.au"
    , "info"
    , "biz"
    , "mil"
    , "co.jp"
    , "sg"
    , "hu"
    ]


{-| Find closest domain in given list of domains and threshold using default distance.

        findClosestDomain "test@gmail.co" slds tlds

is equivalent to

        findClosestDomainWith sift3Distance topLevelThreshold "test@gmail.co" slds tlds

-}
findClosestDomain : String -> List String -> Maybe String
findClosestDomain =
    findClosestDomainWith sift3Distance domainThreshold


{-| Find closest domain in given list of domains using the
given distance and threshold parameters.
-}
findClosestDomainWith : (String -> String -> Float) -> Float -> String -> List String -> Maybe String
findClosestDomainWith distance threshold domain domains =
    if String.isEmpty domain then
        Nothing

    else if List.isEmpty domains then
        Nothing

    else
        let
            distances =
                List.map (\d -> ( d, distance domain d )) domains

            minDist dist_ min_ =
                if second dist_ < second min_ then
                    dist_

                else
                    min_

            ( minDomain_, minDist_ ) =
                List.foldr minDist ( "", 99.0 ) distances

            --_ = Debug.log("fcd") (domain, distances, minDomain_, minDist_)
        in
        if String.isEmpty minDomain_ then
            Nothing

        else if minDist_ > threshold then
            Nothing

        else
            Just minDomain_


encodeEmailReplacements : Dict.Dict String String
encodeEmailReplacements =
    Dict.fromList
        [ -- these replace rules from mailcheck.js and it uses encodeURI
          ( "%20", " " )
        , ( "%25", "%" )
        , ( "%5E", "^" )
        , ( "%60", "`" )
        , ( "%7B", "{" )
        , ( "%7C", "|" )
        , ( "%7D", "}" )

        -- extra rules as using percentEncode does more than needed
        , ( "%40", "@" )
        , ( "%2B", "+" )
        , ( "%23", "#" )
        , ( "%24", "$" )
        , ( "%26", "&" )
        , ( "%2F", "/" )
        , ( "%3D", "=" )
        , ( "%3F", "?" )
        , ( "%22", "\"" )
        ]


encodeEmailReplaceRegex : Maybe.Maybe Regex.Regex
encodeEmailReplaceRegex =
    Dict.keys encodeEmailReplacements
        |> List.intersperse "|"
        |> String.concat
        |> Regex.fromString


{-| Encode the email address to prevent XSS but leave in valid
characters, following this official spec:
<http://en.wikipedia.org/wiki/Email_address#Syntax>

This is exported to test it.

encodeURI() will not encode: ~!@#$&\*()=:/,;?+'
Elm's Http.uriEncode actually calls encodeURIComponent

encodeURIComponent() escapes all characters except the
following: alphabetic, decimal digits, - \_ . ! ~ \* ' ( )

Extra rules were added since Elm provides encodeURIComponent() functionality.

        (encodeEmail "<hello>@domain.com") == "%3Chello%3E@domain.com"

-}
encodeEmail : String -> Maybe.Maybe String
encodeEmail email =
    case encodeEmailReplaceRegex of
        Just regex ->
            email
                |> percentEncode
                |> Regex.replace
                    -- Regex.All
                    regex
                    (\m ->
                        encodeEmailReplacements
                            |> Dict.get m.match
                            |> Maybe.withDefault m.match
                    )
                |> Just

        Nothing ->
            Nothing
