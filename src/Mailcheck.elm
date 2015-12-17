module Mailcheck
    ( suggest
    , suggestWith
    , findClosestDomain
    , mailParts
    , MailParts
    , encodeEmail
    , defaultDomains
    , defaultTopLevelDomains
    , defaultSecondLevelDomains
    ) where

{-| A library that suggests a correct domain when a user miss spells an email address.
This is a port of this javascript library https://github.com/mailcheck/mailcheck

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
@docs mailParts
@docs findClosestDomain

# Default domain lists used by suggest
@docs defaultDomains
@docs defaultTopLevelDomains
@docs defaultSecondLevelDomains

-}

import Http
import Regex
import String
import Maybe exposing (andThen)

import StringDistance exposing (sift3Distance)

type alias MailParts =
  {
    topLevelDomain : String,
    secondLevelDomain : String,
    domain : String,
    address : String
  }

domainThreshold =  2
secondLevelThreshold = 2
topLevelThreshold = 2

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
suggest : String -> Maybe (String, String, String)
suggest =
    suggestWith defaultDomains defaultSecondLevelDomains defaultTopLevelDomains

otherwise2 : (a -> Maybe b) -> (a -> Maybe b) -> a -> Maybe b
otherwise2 a b context =
  case (a context) of
    Nothing -> (b context)
    x -> x

{-| Suggest with passed in domain lists.

* domains is list of known valid domains
* top level domains is allowed to be empty
* second level domains is allowed to be empty

-}
suggestWith : List String -> List String -> List String -> String -> Maybe (String, String, String)
suggestWith domains secondLevelDomains topLevelDomains email =
  let email' = String.toLower email
      checkPartsNotInList' = checkPartsNotInList secondLevelDomains topLevelDomains
      closestDomain' = closestDomain domains
      closestSecondLevelDomain' = closestSecondLevelDomain secondLevelDomains topLevelDomains
      getResult mailParts = (closestDomain' `otherwise2` closestSecondLevelDomain') mailParts
  in
      (mailParts email') `andThen` checkPartsNotInList' `andThen` getResult

checkPartsNotInList : List String -> List String -> MailParts -> Maybe MailParts
checkPartsNotInList secondLevelDomains topLevelDomains mailParts =
  if not ((List.member mailParts.secondLevelDomain secondLevelDomains) && (List.member mailParts.topLevelDomain topLevelDomains)) then
      Just mailParts
  else
    Nothing

closestDomain : List String -> MailParts -> Maybe (String, String, String)
closestDomain domains mailParts =
  let closestDomain = findClosestDomain domainThreshold mailParts.domain domains
      result closestDomain = Just (mailParts.address, closestDomain, mailParts.address ++ "@" ++ closestDomain)
      isDifferentDomain closestDomain =
        if closestDomain == mailParts.domain then
          Nothing
        else
          Just closestDomain
  in
      closestDomain `andThen` isDifferentDomain `andThen` result


closestSecondLevelDomain : List String -> List String -> MailParts -> Maybe (String, String, String)
closestSecondLevelDomain secondLevelDomains topLevelDomains mailParts =
  let
      findClosest threshold domains =
        findClosestDomain threshold domains
      findResultSld = findClosest secondLevelThreshold mailParts.secondLevelDomain secondLevelDomains
      findResultTld = findClosest topLevelThreshold mailParts.topLevelDomain topLevelDomains
      suggestedDomain =
        case (findResultSld, findResultTld) of
          (Nothing, Nothing) -> mailParts.domain
          (Just closestSld, Nothing) -> closestSld ++ "." ++ mailParts.topLevelDomain
          (Nothing, Just closestTld) -> mailParts.secondLevelDomain ++ "." ++ closestTld
          (Just closestSld, Just closestTld) -> closestSld ++ "." ++ closestTld
  in
      if suggestedDomain == mailParts.domain then
        Nothing

      else
        Just (mailParts.address, suggestedDomain, mailParts.address ++ "@" ++ suggestedDomain)


{-| Split an email address up into components.

This is exported to test it.

```elm
  (mailParts "user") == Nothing
  (mailParts "user@") == Nothing
  (mailParts "user@moo.com") == Just({
                                      topLevelDomain = "user",
                                      secondLevelDomain = "moo.com",
                                      domain = "moo",
                                      address = "com" })
  (mailParts "user@moo.co.uk") == Just({
                                        topLevelDomain = "user",
                                        secondLevelDomain = "moo.com.uk",
                                        domain = "moo",
                                        address = "co.ul" })
```

-}
mailParts : String -> Maybe MailParts
mailParts mail =

  let mailSplit = String.trim mail |> String.split "@" |> List.reverse
      domain = List.head mailSplit |> Maybe.withDefault ""
      address = List.tail mailSplit |> Maybe.withDefault [] |> List.reverse |> String.join "@"
  in
     if List.length mailSplit < 2 || List.member "" mailSplit then
       Nothing
     else
       Just {
         topLevelDomain = topLevelDomain domain,
         secondLevelDomain = secondLevelDomain domain,
         domain = domain,
         address = address
       }

topLevelDomain : String -> String
topLevelDomain domain =
  case (String.split "." domain) of
    x::[] -> x
    _::xs -> String.join "." xs
    _ -> ""

secondLevelDomain : String -> String
secondLevelDomain domain =
  case (String.split "." domain) of
    x::[] -> ""
    x::_ -> x
    _ -> ""

{-| default list of domains used in suggest -}
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


{-| default list of second level domains used in suggest -}
defaultSecondLevelDomains : List String
defaultSecondLevelDomains =
    [ "yahoo"
    , "hotmail"
    , "mail"
    , "live"
    , "outlook"
    , "gmx"
    ]


{-| default list of top level domains used in suggest -}
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
findClosestDomain : Float -> String -> List String -> Maybe String
findClosestDomain threshold = findClosestDomainWith sift3Distance threshold


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
        distances = List.map (\d -> (d, (distance domain d))) domains
        minDist dist' min' = if (snd dist') < (snd min') then dist' else min'
        (minDomain', minDist') = List.foldr minDist ("", 99.0) distances
        --_ = Debug.log("fcd") (domain, distances, minDomain', minDist')
      in
        if String.isEmpty minDomain' then
          Nothing

        else if minDist' > threshold then
          Nothing

        else
          Just minDomain'


{-| Encode the email address to prevent XSS but leave in valid
characters, following this official spec:
http://en.wikipedia.org/wiki/Email_address#Syntax

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
  let
      replace pattern replacement =
        Regex.replace Regex.All (Regex.regex pattern) (\_ -> replacement)
      --en = Http.uriEncode email
      --_ = Debug.log("encoded") en
  in  Http.uriEncode email
        -- these replace rules from mailcheck.js and it uses encodeURI
        |> replace "%20" " "
        |> replace "%25" "%"
        |> replace "%5E" "^"
        |> replace "%60" "`"
        |> replace "%7B" "{"
        |> replace "%7C" "|"
        |> replace "%7D" "}"
         -- extra rules as using encodeURIComponent via Http.uriEncode not encodeURI
        |> replace "%40" "@"
        |> replace "%2B" "+"
        |> replace "%23" "#"
        |> replace "%24" "$"
        |> replace "%26" "&"
        |> replace "%2F" "/"
        |> replace "%3D" "="
        |> replace "%3F" "?"
        |> replace "%22" "\""
