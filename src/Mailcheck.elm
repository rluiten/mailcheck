module Mailcheck
    exposing
        ( suggest
        , suggestWith
        , findClosestDomain
        , splitEmail
        , mailParts
        , MailParts
        , encodeEmail
        , defaultDomains
        , defaultTopLevelDomains
        , defaultSecondLevelDomains
        )

{-| A library that suggests a correct domain when a user miss spells an email address.
This is a port of this javascript library <https://github.com/mailcheck/mailcheck>


## Basic Usage

```elm
    Mailcheck.suggest 'test@gnail.com'
      == Just ("test", "gmail.com", "test@gmail.com")
```

```elm
    Mailcheck.suggest 'test@gsnail.com'
      == Nothing
```


# Create

@docs suggest
@docs suggestWith


# Utility

@docs encodeEmail
@docs splitEmail
@docs mailParts
@docs MailParts
@docs findClosestDomain


# Default domain lists used by suggest

@docs defaultDomains
@docs defaultTopLevelDomains
@docs defaultSecondLevelDomains

-}

import Http
import Maybe
import Regex
import String
import Tuple exposing (second)
import StringDistance exposing (sift3Distance)
import MaybeUtils exposing (thenOneOf)
import Dict


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

```elm
    suggest 'test@gmail.co'
```

is equivalent to

```elm
    suggestWith defaultDomains defaultSecondLevelDomains defaultTopLevelDomains 'test@gmail.co'
```

example

```elm
    (suggest "user@gmil.com")
      == Just ("user", "gmail.com", "user@gmail.com")
```

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
checkPartsNotInList secondLevelDomains topLevelDomains mailParts =
    if not ((List.member mailParts.secondLevelDomain secondLevelDomains) && (List.member mailParts.topLevelDomain topLevelDomains)) then
        Just mailParts
    else
        Nothing


closestDomain : List String -> MailParts -> Maybe ( String, String, String )
closestDomain domains mailParts =
    let
        closestDomain =
            findClosestDomain mailParts.domain domains

        result closestDomain =
            Just ( mailParts.address, closestDomain, mailParts.address ++ "@" ++ closestDomain )

        isDifferentDomain closestDomain =
            if closestDomain == mailParts.domain then
                Nothing
            else
                Just closestDomain
    in
        closestDomain
            |> Maybe.andThen isDifferentDomain
            |> Maybe.andThen result


closestSecondLevelDomain : List String -> List String -> MailParts -> Maybe ( String, String, String )
closestSecondLevelDomain secondLevelDomains topLevelDomains mailParts =
    let
        findClosest threshold domains =
            findClosestDomain domains

        findResultSld =
            findClosest secondLevelThreshold mailParts.secondLevelDomain secondLevelDomains

        findResultTld =
            findClosest topLevelThreshold mailParts.topLevelDomain topLevelDomains

        suggestedDomain =
            case ( findResultSld, findResultTld ) of
                ( Nothing, Nothing ) ->
                    mailParts.domain

                ( Just closestSld, Nothing ) ->
                    closestSld ++ "." ++ mailParts.topLevelDomain

                ( Nothing, Just closestTld ) ->
                    mailParts.secondLevelDomain ++ "." ++ closestTld

                ( Just closestSld, Just closestTld ) ->
                    closestSld ++ "." ++ closestTld
    in
        if suggestedDomain == mailParts.domain then
            Nothing
        else
            Just
                ( mailParts.address
                , suggestedDomain
                , mailParts.address ++ "@" ++ suggestedDomain
                )


{-| Split an email address up into components.

This function has been retained to make it a Minor version change not a Major
and now converts the output of mailparts to this form.

```elm
    (spitEmail "user") == Nothing
    (mailParts "user") == Nothing
    (spitEmail "user@") == Nothing
    (mailParts "user@") == Nothing
    (spitEmail "user@moo.com") == Just("user", "moo.com", "moo", "com")
    (spitEmail "user@moo.co.uk") == Just("user", "moo.com.uk", "moo", "co.uk")
```

-}
splitEmail : String -> Maybe ( String, String, String, String )
splitEmail email =
    Maybe.map
        (\parts ->
            ( parts.topLevelDomain
            , parts.secondLevelDomain
            , parts.domain
            , parts.address
            )
        )
        (mailParts email)


{-| Split an email address up into components.

```elm
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
```

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
    case (String.split "." domain) of
        x :: [] ->
            x

        _ :: xs ->
            String.join "." xs

        _ ->
            ""


secondLevelDomain : String -> String
secondLevelDomain domain =
    case (String.split "." domain) of
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

```elm
    findClosestDomain "test@gmail.co" slds tlds
```

is equivalent to

```elm
    findClosestDomainWith sift3Distance topLevelThreshold "test@gmail.co" slds tlds
```

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
                List.map (\d -> ( d, (distance domain d) )) domains

            minDist dist_ min_ =
                if (second dist_) < (second min_) then
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

        -- extra rules as using encodeURIComponent via Http.uriEncode not encodeURI
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


encodeEmailReplaceRegex : Regex.Regex
encodeEmailReplaceRegex =
    Dict.keys encodeEmailReplacements
        |> List.intersperse "|"
        |> String.concat
        |> Regex.regex


{-| Encode the email address to prevent XSS but leave in valid
characters, following this official spec:
<http://en.wikipedia.org/wiki/Email_address#Syntax>

This is exported to test it.

encodeURI() will not encode: ~!@#$&*()=:/,;?+'
Elm's Http.uriEncode actually calls encodeURIComponent

encodeURIComponent() escapes all characters except the
following: alphabetic, decimal digits, - _ . ! ~ * ' ( )

Extra rules were added since Elm provides encodeURIComponent() functionality.

```elm
    (encodeEmail "<hello>@domain.com") == "%3Chello%3E@domain.com"
```

-}
encodeEmail : String -> String
encodeEmail email =
    email
        |> Http.encodeUri
        |> Regex.replace Regex.All
            encodeEmailReplaceRegex
            (\m ->
                encodeEmailReplacements
                    |> Dict.get m.match
                    |> Maybe.withDefault m.match
            )
