module MailcheckTest exposing (..)

import Expect
import String
import Test exposing (..)
import Tuple exposing (first)
import Mailcheck
    exposing
        ( findClosestDomain
        , mailParts
        , MailParts
        , encodeEmail
        , suggest
        , suggestWith
        )


domains =
    [ "google.com", "gmail.com", "emaildomain.com", "comcast.net", "facebook.com", "msn.com", "gmx.de" ]


topLevelDomains =
    [ "co.uk", "com", "org", "info", "fr" ]


secondLevelDomains =
    [ "yahoo", "hotmail", "mail", "live", "outlook", "gmx" ]


type alias Mailcheck =
    Maybe ( String, String, String )


tests : Test
tests =
    describe "Mailcheck tests"
        [ encodeEmailTests
        , mailPartsTests
        , findClosestDomainTests
        , suggestTests
        ]


findClosestDomainTests : Test
findClosestDomainTests =
    describe "findClosestDomain"
        [ runFindClosestDomainTest "returns the most similar domain" domainsData
        , runFindClosestDomainTest "returns the most similar second-level domain" secondLevelDomainsData
        , runFindClosestDomainTest "returns the most similar top-level domain" topLevelDomainsData
        , runFindClosestDomainTest "limit search and return Nothing as needed" findLimitSearchRange
        ]



-- List expect, input, domains


type alias ExpectedFindClosestDomainExamples =
    List ( Maybe String, String, List String )


domainsData : ExpectedFindClosestDomainExamples
domainsData =
    [ ( Just "google.com", "google.com", domains )
    , ( Just "gmail.com", "gmail.com", domains )
    , ( Just "emaildomain.com", "emaildoman.com", domains )
    , ( Just "msn.com", "gmsn.com", domains )
    , ( Just "gmail.com", "gmaik.com", domains )
    ]


secondLevelDomainsData : ExpectedFindClosestDomainExamples
secondLevelDomainsData =
    [ ( Just "hotmail", "hotmial", secondLevelDomains )
    , ( Just "yahoo", "tahoo", secondLevelDomains )
    , ( Just "live", "livr", secondLevelDomains )
    , ( Just "outlook", "outllok", secondLevelDomains )
    ]


topLevelDomainsData : ExpectedFindClosestDomainExamples
topLevelDomainsData =
    [ ( Just "com", "cmo", topLevelDomains )
    , ( Just "org", "ogr", topLevelDomains )
    , ( Just "info", "ifno", topLevelDomains )
    , ( Just "co.uk", "com.uk", topLevelDomains )
    ]


limitSearchStrings =
    [ "xxxxxx"
    , "ovenmat"
    ]


findLimitSearchRange : ExpectedFindClosestDomainExamples
findLimitSearchRange =
    [ ( Just "ovenmat", "oven", limitSearchStrings )
    , ( Nothing, "cat", limitSearchStrings )
    , ( Nothing, "ve", limitSearchStrings )
    , ( Just "ovenmat", "ven", limitSearchStrings )
    , ( Just "ovenmat", "nma", limitSearchStrings )
    , ( Just "ovenmat", "xenma", limitSearchStrings )
    , ( Nothing, "mate", limitSearchStrings )
    ]


runFindClosestDomainTest : String -> ExpectedFindClosestDomainExamples -> Test
runFindClosestDomainTest name data =
    describe name <|
        List.map
            (\( expect, input, domains ) ->
                -- input String doubles as test name
                test input <|
                    \() ->
                        (Expect.equal
                            expect
                            (findClosestDomain input domains)
                        )
            )
            data


mailPartsTests : Test
mailPartsTests =
    describe "mailParts"
        [ runMailPartsTest "returns a MailParts with of the address, the domain, and the top-level domain" mailPartsData
        , runMailPartsTest "splits RFC compliant emails" splitRfcCompliantData
        , runMailPartsTest "returns Nothing for email addresses that are not RFC compliant" splitNonRfcCompliantData
        , runMailPartsTest "trims spaces from the start and end of the string" splitTrimsSpacesData
        ]



-- List (email, (split result))


type alias ExpectedSplitEmailExamples =
    List ( String, Maybe MailParts )


mailPartsInit : String -> String -> String -> String -> Maybe MailParts
mailPartsInit address domain sld tld =
    Just
        { topLevelDomain = tld
        , secondLevelDomain = sld
        , domain = domain
        , address = address
        }


mailPartsData : ExpectedSplitEmailExamples
mailPartsData =
    [ ( "test@example.com"
      , (mailPartsInit "test" "example.com" "example" "com")
      )
    , ( "test@example.co.uk"
      , (mailPartsInit "test" "example.co.uk" "example" "co.uk")
      )
    , ( "test@mail.randomsmallcompany.co.uk"
      , (mailPartsInit "test" "mail.randomsmallcompany.co.uk" "mail" "randomsmallcompany.co.uk")
      )
    ]


splitRfcCompliantData : ExpectedSplitEmailExamples
splitRfcCompliantData =
    [ ( "\"foo@bar\"@example.com"
      , mailPartsInit "\"foo@bar\"" "example.com" "example" "com"
      )
    , ( "containsnumbers1234567890@example.com"
      , mailPartsInit "containsnumbers1234567890" "example.com" "example" "com"
      )
    , ( "contains+symbol@example.com"
      , mailPartsInit "contains+symbol" "example.com" "example" "com"
      )
    , ( "contains-symbol@example.com"
      , mailPartsInit "contains-symbol" "example.com" "example" "com"
      )
    , ( "contains.symbol@domain.contains.symbol"
      , mailPartsInit "contains.symbol" "domain.contains.symbol" "domain" "contains.symbol"
      )

    {- Original mailcheck.js test
         expect(mailcheck.splitEmail('"contains.and\ symbols"@example.com')).toEqual({
           address:'"contains.and\ symbols"',
           domain:'example.com',
           topLevelDomain:'com',
           secondLevelDomain: 'example'
         });
       Modifications to test relative to original mailcheck.js are as follows
       * space is not escaped in Elm by '\' as it has different behaviour to javascript
    -}
    , ( "\"contains.and symbols\"@example.com"
      , mailPartsInit "\"contains.and symbols\"" "example.com" "example" "com"
      )
    , ( "\"contains.and.@.symbols.com\"@example.com"
      , mailPartsInit "\"contains.and.@.symbols.com\"" "example.com" "example" "com"
      )

    {- Original mailcheck.js test
         expect(mailcheck.splitEmail('"()<>[]:;@,\\\"!#$%&\'*+-/=?^_`{}|\ \ \ \ \ ~\ \ \ \ \ \ \ ?\ \ \ \ \ \ \ \ \ \ \ \ ^_`{}|~.a"@allthesymbols.com')).toEqual({
           address:'"()<>[]:;@,\\\"!#$%&\'*+-/=?^_`{}|\ \ \ \ \ ~\ \ \ \ \ \ \ ?\ \ \ \ \ \ \ \ \ \ \ \ ^_`{}|~.a"',
           domain:'allthesymbols.com',
           topLevelDomain:'com',
           secondLevelDomain: 'allthesymbols'
         });
       Modifications to test relative to original mailcheck.js are as follows
       * space is not escaped in Elm by '\' as it has different behaviour to javascript
       * backticks are not escaped by '\' in Elm
    -}
    , ( "\"()<>[]:;@,\\\"!#$%&'*+-/=?^_`{}|     ~       ?            ^_`{}|~.a\"@allthesymbols.com"
      , mailPartsInit "\"()<>[]:;@,\\\"!#$%&'*+-/=?^_`{}|     ~       ?            ^_`{}|~.a\""
            "allthesymbols.com"
            "allthesymbols"
            "com"
      )
    , ( "postbox@com"
      , mailPartsInit "postbox" "com" "" "com"
      )
    ]


splitNonRfcCompliantData : ExpectedSplitEmailExamples
splitNonRfcCompliantData =
    [ ( "example.com", Nothing )
    , ( "abc.example.com", Nothing )
    , ( "@example.com", Nothing )
    , ( "test@", Nothing )
    ]


splitTrimsSpacesData : ExpectedSplitEmailExamples
splitTrimsSpacesData =
    [ ( " postbox@com"
      , mailPartsInit "postbox" "com" "" "com"
      )
    , ( "postbox@com "
      , mailPartsInit "postbox" "com" "" "com"
      )
    ]


runMailPartsTest : String -> ExpectedSplitEmailExamples -> Test
runMailPartsTest name data =
    let
        tests =
            List.map
                (\( email, expect ) ->
                    -- name String doubles as test name
                    test name <|
                        \() ->
                            (Expect.equal
                                expect
                                (mailParts email)
                            )
                )
                data
    in
        describe name tests


encodeEmailTests =
    describe "encodeEmail tests"
        [ runEncodeEmailTest "example cases" encodeEmailExamples ]



-- List (input, output)


type alias ExpectedEncodeEmailExamples =
    List ( String, String )


encodeEmailExamples : ExpectedEncodeEmailExamples
encodeEmailExamples =
    [ ( "hello@test.com", "hello@test.com" )
    , ( "hello+more@test.com", "hello+more@test.com" )
    , ( "hello+more(comment)@test.com", "hello+more(comment)@test.com" )
    , ( "(comment)hello+more@test.com", "(comment)hello+more@test.com" )
    , ( "a !#$%&'*+-/=?^_`{|}~\"@test.com", "a !#$%&'*+-/=?^_`{|}~\"@test.com" )
    , ( "<script>alert(\"a\")</xscript>@emaildomain.con"
      , "%3Cscript%3Ealert(\"a\")%3C/xscript%3E@emaildomain.con"
      )

    -- Elm 0.16 Compiler broken for </script> in string, so splitting strings.
    -- , ( "<script>alert(\"a\")<" ++ "/script>@emaildomain.con", "%3Cscript%3Ealert(\"a\")%3C/script%3E@emaildomain.con" )
    -- Elm 0.18 fixes bit does not need the split of </script> in above line.
    , ( "<script>alert(\"a\")</script>@emaildomain.con", "%3Cscript%3Ealert(\"a\")%3C/script%3E@emaildomain.con" )
    ]


runEncodeEmailTest : String -> ExpectedEncodeEmailExamples -> Test
runEncodeEmailTest name data =
    let
        tests =
            List.map
                (\( input, expect ) ->
                    test ("Test case " ++ input)
                        (\() ->
                            (Expect.equal
                                expect
                                (encodeEmail input)
                            )
                        )
                )
                data
    in
        describe name tests


suggestTests : Test
suggestTests =
    runSuggestTest "suggest cases" suggestionData


type SuggestCase
    = AnonSuggestCase ( String -> Mailcheck, String, Mailcheck )
    | NamedSuggestCase ( String, String -> Mailcheck, String, Mailcheck )


type alias ExpectedSuggestExamples =
    List SuggestCase


suggestionData : ExpectedSuggestExamples
suggestionData =
    [ AnonSuggestCase
        ( suggest
        , "test@gmail.co"
        , Just ( "test", "gmail.com", "test@gmail.com" )
        )
    , NamedSuggestCase
        ( "takes in an array of specified domains"
        , suggestWith domains [] []
        , "test@emaildomain.con"
        , Just ( "test", "emaildomain.com", "test@emaildomain.com" )
        )
    , NamedSuggestCase
        ( "domain not close to any domain in default domain list test@emaildomain.con but tld found"
        , suggest
        , "test@emaildomain.con"
        , Just ( "test", "emaildomain.co.nz", "test@emaildomain.co.nz" )
        )
    , NamedSuggestCase
        ( "custom domain list is near to misspelled domain"
        , suggestWith domains [] []
        , "test@xmaildomain.con"
        , Just ( "test", "emaildomain.com", "test@emaildomain.com" )
        )
    , AnonSuggestCase
        ( suggest
        , "contact@kicksend.com"
        , Nothing
        )
    , NamedSuggestCase
        ( "no suggestion if incomplete email provided"
        , suggest
        , "contact"
        , Nothing
        )
    , AnonSuggestCase
        ( suggest
        , "test@gmailc.om"
        , Just ( "test", "gmail.com", "test@gmail.com" )
        )
    , AnonSuggestCase
        ( suggestWith domains [] []
        , "test@emaildomain.co"
        , Just ( "test", "emaildomain.com", "test@emaildomain.com" )
        )
    , AnonSuggestCase
        ( suggestWith domains [] []
        , "test@gnail.con"
        , Just ( "test", "gmail.com", "test@gmail.com" )
        )
    , AnonSuggestCase
        ( suggestWith domains [] []
        , "test@gsnail.con"
        , Nothing
        )
    , AnonSuggestCase
        ( suggestWith domains [] []
        , "test@GNAIL.con"
        , Just ( "test", "gmail.com", "test@gmail.com" )
        )
    , AnonSuggestCase
        ( suggestWith domains [] []
        , "test@#gmail.com"
        , Just ( "test", "gmail.com", "test@gmail.com" )
        )
    , AnonSuggestCase
        ( suggestWith domains [] []
        , "test@comcast.nry"
        , Just ( "test", "comcast.net", "test@comcast.net" )
        )
    , AnonSuggestCase
        ( suggestWith domains secondLevelDomains topLevelDomains
        , "test@homail.con"
        , Just ( "test", "hotmail.com", "test@hotmail.com" )
        )
    , AnonSuggestCase
        ( suggestWith domains secondLevelDomains topLevelDomains
        , "test@hotmail.co"
        , Just ( "test", "hotmail.com", "test@hotmail.com" )
        )
    , AnonSuggestCase
        ( suggestWith domains secondLevelDomains topLevelDomains
        , "test@yajoo.com"
        , Just ( "test", "yahoo.com", "test@yahoo.com" )
        )
    , AnonSuggestCase
        ( suggestWith domains secondLevelDomains topLevelDomains
        , "test@randomsmallcompany.cmo"
        , Just ( "test", "randomsmallcompany.com", "test@randomsmallcompany.com" )
        )
    , NamedSuggestCase
        ( "empty email address produces NoSuggestion"
        , suggestWith domains secondLevelDomains topLevelDomains
        , ""
        , Nothing
        )
    , NamedSuggestCase
        ( "missing email address domain produces NoSuggestion"
        , suggestWith domains secondLevelDomains topLevelDomains
        , "test@"
        , Nothing
        )
    , NamedSuggestCase
        ( "not @ or domain in email produces NoSuggestion"
        , suggestWith domains secondLevelDomains topLevelDomains
        , "test@"
        , Nothing
        )
    , NamedSuggestCase
        ( "this test is for illustrative purposes as the splitEmail function should return a better representation of the true top-level domain in the case of an email address with subdomains. mailcheck will be unable to return a suggestion in the case of this email address"
        , suggestWith domains secondLevelDomains topLevelDomains
        , "test@mail.randomsmallcompany.cmo"
        , Nothing
        )
    , NamedSuggestCase
        ( "will not offer a suggestion that itself leads to another suggestion"
        , suggestWith domains secondLevelDomains topLevelDomains
        , "test@yahooo.cmo"
        , Just ( "test", "yahoo.com", "test@yahoo.com" )
        )
    , NamedSuggestCase
        ( "will not offer suggestions for valid 2ld-tld combinations"
        , suggestWith domains secondLevelDomains topLevelDomains
        , "test@yahoo.co.uk"
        , Nothing
        )
    , NamedSuggestCase
        ( "will not offer suggestions for valid 2ld-tld even if theres a close fully-specified domain"
        , suggestWith domains secondLevelDomains topLevelDomains
        , "test@gmx.fr"
        , Nothing
        )
    , NamedSuggestCase
        ( "an example used in readme"
        , suggestWith [ "yohomail.com" ] [ "supamail" ] [ "cosmic" ]
        , "test@ohomail.co"
        , Just ( "test", "yohomail.com", "test@yohomail.com" )
        )
    , NamedSuggestCase
        ( "an example used in readme"
        , suggestWith [ "yohomail.com" ] [ "supamail" ] [ "cosmic" ]
        , "test@fakedomain.comic"
        , Just ( "test", "fakedomain.cosmic", "test@fakedomain.cosmic" )
        )
    , NamedSuggestCase
        ( "an example used in readme"
        , suggestWith [ "yohomail.com" ] [ "supamail" ] [ "cosmic" ]
        , "test@supermail.comic"
        , Just ( "test", "supamail.cosmic", "test@supamail.cosmic" )
        )
    ]


runSuggestTest : String -> ExpectedSuggestExamples -> Test
runSuggestTest name data =
    let
        tests =
            List.map
                (\caseData ->
                    let
                        ( name, suggest, input, expect ) =
                            case caseData of
                                AnonSuggestCase ( suggest_, input_, expect_ ) ->
                                    ( input_, suggest_, input_, expect_ )

                                NamedSuggestCase ( name_, suggest_, input_, expect_ ) ->
                                    ( name_, suggest_, input_, expect_ )
                    in
                        test name <|
                            \() ->
                                (Expect.equal
                                    expect
                                    (suggest input)
                                )
                )
                data
    in
        describe name tests
