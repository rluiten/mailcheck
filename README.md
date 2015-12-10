Mailcheck
=========

An Elm library to suggest a correct domain when users misspell email addresses.

This is a port of mailcheck.js at  https://github.com/mailcheck/mailcheck


What does it do?
----------------

When your user types in "user@gmil.con", it will suggest "user@gmail.com".

It is currently worth looking at the documentation at https://github.com/mailcheck/mailcheck

Installation
------------

```
    elm package install rluiten/elm-mailcheck
```

Testing
-------

See tests directory.

Execute TestRunner.elm in tests folder to run tests.


Example1.elm
------------

```elm
import Mailcheck exposing (suggest)


input1 = "user@gmil.com"
mailcheckResult1 = suggest input1
test1Pass = mailcheckResult1 == Just ("user", "gmail.com", "user@gmail.com")


-- input2 = "user@gsnail.com"
input2 = "user@googlemail.com"
mailcheckResult2 = suggest input2
test2Pass = mailcheckResult2 == Nothing


_ = Debug.log("mailcheckResult1") (input1, mailcheckResult1, test1Pass)
_ = Debug.log("mailcheckResult2") (input2, mailcheckResult2, test2Pass)

```

Example2.elm
------------

It is possible to provide your own list of domains, second level domains and top level domains.

```elm
import Mailcheck exposing (suggestWith)

domains = [ "yohomail.com" ]
secondLevelDomains = [ "supamail" ]
topLevelDomains = [ "cosmic" ]


input1 = "test@ohomail.co"
mailcheckResult1 =
    suggestWith domains secondLevelDomains topLevelDomains input1
test1Pass =
    mailcheckResult1 == Just ("test", "yohomail.com", "test@yohomail.com")


input2 = "test@fakedomain.comic"
mailcheckResult2 =
    suggestWith domains secondLevelDomains topLevelDomains input2
test2Pass =
    mailcheckResult2 == Just ("test", "fakedomain.cosmic", "test@fakedomain.cosmic")


input3 = "test@supermail.tld"
mailcheckResult3 =
    suggestWith domains secondLevelDomains topLevelDomains input3
test3Pass =
    mailcheckResult3 == Just ("test", "supamail.tld", "test@supamail.tld")


_ = Debug.log("mailcheckResult1") (input1, mailcheckResult1, test1Pass)
_ = Debug.log("mailcheckResult2") (input2, mailcheckResult2, test2Pass)
_ = Debug.log("mailcheckResult3") (input3, mailcheckResult3, test3Pass)

```



License
-------

Released under the BSD3 License.
