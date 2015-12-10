module Mailcheck
    ( suggest
    , suggestWith
    , findClosestDomain
    , splitEmail
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

# Utility
@docs encodeEmail
@docs splitEmail
@docs findClosestDomain
@docs suggestWith

# Default domain lists used by suggest
@docs defaultDomains
@docs defaultTopLevelDomains
@docs defaultSecondLevelDomains

-}

import Http
import Regex
import String

import StringDistance exposing (sift3Distance)


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

```elm
  (suggest "user@gmil.com") == Just ("user", "gmail.com", "user@gmail.com")
```

-}
suggest : String -> Maybe (String, String, String)
suggest =
    suggestWith defaultDomains defaultSecondLevelDomains defaultTopLevelDomains


{-| Suggest with passed in domain lists.

domains is list of known valid domains
top level domains allowed to be empty
second level domains allowed to be empty

-}
suggestWith : List String -> List String -> List String -> String -> Maybe (String, String, String)
suggestWith domains secondLevelDomains topLevelDomains email =
    let
      splitEmail' = splitEmail(String.toLower email)
    in
      case splitEmail' of
        Nothing -> Nothing
        Just (address', domain', secondLevelDomain', topLevelDomain') ->
          suggestWith' domains secondLevelDomains topLevelDomains address' domain' secondLevelDomain' topLevelDomain'


domainThreshold =  2


suggestWith'
  :  List String -> List String -> List String
  -> String -> String -> String -> String
  -> Maybe (String, String, String)
suggestWith' domains secondLevelDomains topLevelDomains address domain sld tld =
    if  (List.member sld secondLevelDomains) && (List.member tld topLevelDomains) then
        Nothing

    else
      let
        findResult = findClosestDomainWith sift3Distance domainThreshold domain domains
      in
        case findResult of
          Nothing ->
            buildSuggest topLevelDomains secondLevelDomains address domain tld sld
          Just (closestDomain) ->
            if domain == closestDomain then
              Nothing

            else
              Just (address, closestDomain, address ++ "@" ++ closestDomain)


buildSuggest topLevelDomains secondLevelDomains address domain tld sld =
    let
      secondLevelThreshold = 2
      topLevelThreshold = 2
      findSld = findClosestDomainWith sift3Distance secondLevelThreshold
      findResultSld = findSld sld secondLevelDomains
      findTld = findClosestDomainWith sift3Distance topLevelThreshold
      findResultTld = findTld tld topLevelDomains
      suggestedDomain =
        case (findResultSld, findResultTld) of
          (Nothing, Nothing) -> domain
          (Just closestSld, Nothing) -> closestSld ++ "." ++ tld
          (Nothing, Just closestTld) -> sld ++ "." ++ closestTld
          (Just closestSld, Just closestTld) -> closestSld ++ "." ++ closestTld
    in
      if suggestedDomain == domain then
        Nothing

      else
        Just (address, suggestedDomain, address ++ "@" ++ suggestedDomain)

{-| Split an email address up into components.
This is exported to test it.
Result is Maybe (address, domain, secondLevelDomain, topLevelDomain)

```elm
  (spitEmail "user") == Nothing
  (spitEmail "user@") == Nothing
  (spitEmail "user@moo.com") == Just("user", "moo.com", "moo", "com")
  (spitEmail "user@moo.co.uk") == Just("user", "moo.com.uk", "moo", "co.uk")
```

-}
splitEmail : String -> Maybe (String, String, String, String)
splitEmail email =
    let
      -- reverse so have domain at head of list
      reverseSplitEmail =
        List.reverse <| String.split "@" <| String.trim email
    in
      if List.any (\p -> String.isEmpty p) reverseSplitEmail then
        Nothing

      else
        case reverseSplitEmail of
          [] -> Nothing
          _ :: [] -> Nothing
          domain :: rest ->
            let address = String.join "@" <| List.reverse rest
                --_ = Debug.log("domain") (domain, rest, address, reverseSplitEmail)
            in  splitDomain address domain


splitDomain address domain =
    let
      splitDomain = String.split "." domain
    in
      case splitDomain of
        -- The address does not have a top-level domain
        [] -> Nothing

        -- The address has only a top-level domain (valid under RFC)
        top :: [] -> Just (address, domain, "", top)

        secondLevelDomain :: restDomains ->
          let topLevelDomain = String.join "." restDomains
          in  Just (address, domain, secondLevelDomain, topLevelDomain)


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


{-| Find closest domain in given list of domains using default distance and threshold.

```elm
    findClosestDomain "test@gmail.co" slds tlds
```

is equivalent to

```elm
    findClosestDomainWith sift3Distance topLevelThreshold "test@gmail.co" slds tlds
```
-}
findClosestDomain : String -> List String -> Maybe String
findClosestDomain = findClosestDomainWith sift3Distance domainThreshold


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

encodeURIComponent escapes all characters except the
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
