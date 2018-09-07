Mailcheck
=========

An Elm library to suggest a correct domain when users misspell email addresses.

This is a port of mailcheck.js at  https://github.com/mailcheck/mailcheck


What does it do?
----------------

When your user types in "user@gmil.con", it will suggest "user@gmail.com".

It is currently worth looking at the documentation at https://github.com/mailcheck/mailcheck

4.1.0 2015/12/30

    Thanks to https://github.com/Adrian2112 for a pull request, he created the
    `mailParts` function. I have retained `splitEmail` in API to make the version
    change a Minor, but it now just uses the mailParts logic. Adrian also
    contributed a few refactors I have pulled in manually.

5.0.0 2-18/09/07

    Updated to Elm 0.19.
    Removed splitEmail.

Installation
------------

```
    elm package install rluiten/elm-mailcheck
```

## Testing

This uses elm-test for testing so install it if you dont have it.

* npm install -g elm-test

To run Tests

* elm-test

Copyright (c) 2016 Robin Luiten

Example1.elm
------------

```elm
module Main exposing (input1, input2, mailcheckResult1, mailcheckResult2, test1Pass, test2Pass)

import Mailcheck exposing (suggest)


input1 =
    "user@gmil.com"


mailcheckResult1 =
    suggest input1


test1Pass =
    mailcheckResult1 == Just ( "user", "gmail.com", "user@gmail.com" )


input2 =
    {- "user@gsnail.com" -}
    "user@googlemail.com"


mailcheckResult2 =
    suggest input2


test2Pass =
    mailcheckResult2 == Nothing


d1 =
    Debug.log "mailcheckResult1" ( input1, mailcheckResult1, test1Pass )


d2 =
    Debug.log "mailcheckResult2" ( input2, mailcheckResult2, test2Pass )
```

Example2.elm
------------

It is possible to provide your own list of domains, second level domains and top level domains.

```elm
module Main exposing (domains, input1, input2, input3, mailcheckResult1, mailcheckResult2, mailcheckResult3, secondLevelDomains, test1Pass, test2Pass, test3Pass, topLevelDomains)

import Mailcheck exposing (suggestWith)


domains =
    [ "yohomail.com" ]


secondLevelDomains =
    [ "supamail" ]


topLevelDomains =
    [ "cosmic" ]


input1 =
    "test@ohomail.co"


mailcheckResult1 =
    suggestWith domains secondLevelDomains topLevelDomains input1


test1Pass =
    mailcheckResult1 == Just ( "test", "yohomail.com", "test@yohomail.com" )


input2 =
    "test@fakedomain.comic"


mailcheckResult2 =
    suggestWith domains secondLevelDomains topLevelDomains input2


test2Pass =
    mailcheckResult2 == Just ( "test", "fakedomain.cosmic", "test@fakedomain.cosmic" )


input3 =
    "test@supermail.tld"


mailcheckResult3 =
    suggestWith domains secondLevelDomains topLevelDomains input3


test3Pass =
    mailcheckResult3 == Just ( "test", "supamail.tld", "test@supamail.tld" )


_ =
    Debug.log "mailcheckResult1" ( input1, mailcheckResult1, test1Pass )


_ =
    Debug.log "mailcheckResult2" ( input2, mailcheckResult2, test2Pass )


_ =
    Debug.log "mailcheckResult3" ( input3, mailcheckResult3, test3Pass )
```



License
-------

Released under the BSD3 License.

(c) 2015 Robin Luiten
