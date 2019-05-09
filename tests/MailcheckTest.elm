module MailcheckTest exposing (ExpectedEncodeEmailExamples, ExpectedFindClosestDomainExamples, ExpectedSplitEmailExamples, ExpectedSuggestExamples, SuggestCase(..), domains, domainsData, encodeEmailExamples, encodeEmailTests, findClosestDomainTests, findLimitSearchRange, limitSearchStrings, mailPartsData, mailPartsInit, mailPartsTests, runEncodeEmailTest, runFindClosestDomainTest, runMailPartsTest, runSuggestTest, secondLevelDomains, secondLevelDomainsData, splitNonRfcCompliantData, splitRfcCompliantData, splitTrimsSpacesData, suggestTests, suggestionData, topLevelDomains, topLevelDomainsData)

import Expect
import Mailcheck
    exposing
        ( MailParts
        , encodeEmail
        , findClosestDomain
        , mailParts
        , suggest
        , suggestWith
        )
import String
import Test exposing (..)
import Tuple exposing (first)


domains =
    [ "google.com", "gmail.com", "emaildomain.com", "comcast.net", "facebook.com", "msn.com", "gmx.de" ]


topLevelDomains =
    [ "co.uk", "com", "org", "info", "fr" ]


secondLevelDomains =
    [ "yahoo", "hotmail", "mail", "live", "outlook", "gmx" ]


type alias Mailcheck =
    Maybe ( String, String, String )


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
            (\( expect, input, domainsX ) ->
                -- input String doubles as test name
                test input <|
                    \() ->
                        Expect.equal
                            expect
                            (findClosestDomain input domainsX)
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
      , mailPartsInit "test" "example.com" "example" "com"
      )
    , ( "test@example.co.uk"
      , mailPartsInit "test" "example.co.uk" "example" "co.uk"
      )
    , ( "test@mail.randomsmallcompany.co.uk"
      , mailPartsInit "test" "mail.randomsmallcompany.co.uk" "mail" "randomsmallcompany.co.uk"
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
runMailPartsTest testSetName data =
    let
        testSet =
            data
                |> List.map
                    (\( email, expect ) ->
                        -- name String doubles as test name
                        test (testSetName ++ email) <|
                            \() ->
                                Expect.equal
                                    expect
                                    (mailParts email)
                    )
    in
    describe testSetName testSet


encodeEmailTests =
    describe "encodeEmail tests"
        [ runEncodeEmailTest "example cases" encodeEmailExamples ]



-- List (input, output)


type alias ExpectedEncodeEmailExamples =
    List ( String, Maybe String )


encodeEmailExamples : ExpectedEncodeEmailExamples
encodeEmailExamples =
    [ ( "hello@test.com", Just "hello@test.com" )

    -- , ( "abc?@test.com", Just "mooooooo@test.com" )
    , ( "hello+more@test.com", Just "hello+more@test.com" )
    , ( "hello+more(comment)@test.com", Just "hello+more(comment)@test.com" )
    , ( "(comment)hello+more@test.com", Just "(comment)hello+more@test.com" )
    , ( "a !#$%&'*+-/=?^_`{|}~\"@test.com", Just "a !#$%&'*+-/=?^_`{|}~\"@test.com" )
    , ( "<script>alert(\"a\")</xscript>@emaildomain.con"
      , Just "%3Cscript%3Ealert(\"a\")%3C/xscript%3E@emaildomain.con"
      )

    -- Elm 0.16 Compiler broken for </script> in string, so splitting strings.
    -- , ( "<script>alert(\"a\")<" ++ "/script>@emaildomain.con", "%3Cscript%3Ealert(\"a\")%3C/script%3E@emaildomain.con" )
    -- Elm 0.18 fixes bit does not need the split of </script> in above line.
    , ( "<script>alert(\"a\")</script>@emaildomain.con"
      , Just "%3Cscript%3Ealert(\"a\")%3C/script%3E@emaildomain.con"
      )
    ]


runEncodeEmailTest : String -> ExpectedEncodeEmailExamples -> Test
runEncodeEmailTest testSetName data =
    let
        testSet =
            List.map
                (\( input, expect ) ->
                    test ("Test case " ++ input)
                        (\() ->
                            Expect.equal
                                expect
                                (encodeEmail input)
                        )
                )
                data
    in
    describe testSetName testSet


suggestTests : Test
suggestTests =
    runSuggestTest "suggest cases" suggestionData


type alias AnonSuggestCaseRecord =
    { f : String -> Mailcheck
    , input : String
    , expected : Mailcheck
    }


type alias NamedSuggestCaseRecord =
    { name : String
    , f : String -> Mailcheck
    , input : String
    , expected : Mailcheck
    }


type SuggestCase
    = Anon AnonSuggestCaseRecord
    | Named NamedSuggestCaseRecord


type alias ExpectedSuggestExamples =
    List SuggestCase


suggestionData : ExpectedSuggestExamples
suggestionData =
    [ Anon
        { f = suggest
        , input = "test@gmail.co"
        , expected = Just ( "test", "gmail.com", "test@gmail.com" )
        }
    , Named
        { name = "takes in an array of specified domains"
        , f = suggestWith domains [] []
        , input = "test@emaildomain.con"
        , expected = Just ( "test", "emaildomain.com", "test@emaildomain.com" )
        }
    , Named
        { name = "domain not close to any domain in default domain list test@emaildomain.con but tld found"
        , f = suggest
        , input = "test@emaildomain.con"
        , expected = Just ( "test", "emaildomain.co.nz", "test@emaildomain.co.nz" )
        }
    , Named
        { name = "custom domain list is near to misspelled domain"
        , f = suggestWith domains [] []
        , input = "test@xmaildomain.con"
        , expected = Just ( "test", "emaildomain.com", "test@emaildomain.com" )
        }
    , Anon
        { f = suggest
        , input = "contact@kicksend.com"
        , expected = Nothing
        }
    , Named
        { name = "no suggestion if incomplete email provided"
        , f = suggest
        , input = "contact"
        , expected = Nothing
        }
    , Anon
        { f = suggest
        , input = "test@gmailc.om"
        , expected = Just ( "test", "gmail.com", "test@gmail.com" )
        }
    , Anon
        { f = suggestWith domains [] []
        , input = "test@emaildomain.co"
        , expected = Just ( "test", "emaildomain.com", "test@emaildomain.com" )
        }
    , Anon
        { f = suggestWith domains [] []
        , input = "test@gnail.con"
        , expected = Just ( "test", "gmail.com", "test@gmail.com" )
        }
    , Anon
        { f = suggestWith domains [] []
        , input = "test@gsnail.con"
        , expected = Nothing
        }
    , Anon
        { f = suggestWith domains [] []
        , input = "test@GNAIL.con"
        , expected = Just ( "test", "gmail.com", "test@gmail.com" )
        }
    , Anon
        { f = suggestWith domains [] []
        , input = "test@#gmail.com"
        , expected = Just ( "test", "gmail.com", "test@gmail.com" )
        }
    , Anon
        { f = suggestWith domains [] []
        , input = "test@comcast.nry"
        , expected = Just ( "test", "comcast.net", "test@comcast.net" )
        }
    , Anon
        { f = suggestWith domains secondLevelDomains topLevelDomains
        , input = "test@homail.con"
        , expected = Just ( "test", "hotmail.com", "test@hotmail.com" )
        }
    , Anon
        { f = suggestWith domains secondLevelDomains topLevelDomains
        , input = "test@hotmail.co"
        , expected = Just ( "test", "hotmail.com", "test@hotmail.com" )
        }
    , Anon
        { f = suggestWith domains secondLevelDomains topLevelDomains
        , input = "test@yajoo.com"
        , expected = Just ( "test", "yahoo.com", "test@yahoo.com" )
        }
    , Anon
        { f = suggestWith domains secondLevelDomains topLevelDomains
        , input = "test@randomsmallcompany.cmo"
        , expected = Just ( "test", "randomsmallcompany.com", "test@randomsmallcompany.com" )
        }
    , Named
        { name = "empty email address produces NoSuggestion"
        , f = suggestWith domains secondLevelDomains topLevelDomains
        , input = ""
        , expected = Nothing
        }
    , Named
        { name = "missing email address domain produces NoSuggestion"
        , f = suggestWith domains secondLevelDomains topLevelDomains
        , input = "test@"
        , expected = Nothing
        }
    , Named
        { name = "not @ or domain in email produces NoSuggestion"
        , f = suggestWith domains secondLevelDomains topLevelDomains
        , input = "test@"
        , expected = Nothing
        }
    , Named
        { name = "this test is for illustrative purposes as the splitEmail function should return a better representation of the true top-level domain in the case of an email address with subdomains. mailcheck will be unable to return a suggestion in the case of this email address"
        , f = suggestWith domains secondLevelDomains topLevelDomains
        , input = "test@mail.randomsmallcompany.cmo"
        , expected = Nothing
        }
    , Named
        { name = "will not offer a suggestion that itself leads to another suggestion"
        , f = suggestWith domains secondLevelDomains topLevelDomains
        , input = "test@yahooo.cmo"
        , expected = Just ( "test", "yahoo.com", "test@yahoo.com" )
        }
    , Named
        { name = "will not offer suggestions for valid 2ld-tld combinations"
        , f = suggestWith domains secondLevelDomains topLevelDomains
        , input = "test@yahoo.co.uk"
        , expected = Nothing
        }
    , Named
        { name = "will not offer suggestions for valid 2ld-tld even if theres a close fully-specified domain"
        , f = suggestWith domains secondLevelDomains topLevelDomains
        , input = "test@gmx.fr"
        , expected = Nothing
        }
    , Named
        { name = "an example used in readme 1"
        , f = suggestWith [ "yohomail.com" ] [ "supamail" ] [ "cosmic" ]
        , input = "test@ohomail.co"
        , expected = Just ( "test", "yohomail.com", "test@yohomail.com" )
        }
    , Named
        { name = "an example used in readme 2"
        , f = suggestWith [ "yohomail.com" ] [ "supamail" ] [ "cosmic" ]
        , input = "test@fakedomain.comic"
        , expected = Just ( "test", "fakedomain.cosmic", "test@fakedomain.cosmic" )
        }
    , Named
        { name = "an example used in readme 3"
        , f = suggestWith [ "yohomail.com" ] [ "supamail" ] [ "cosmic" ]
        , input = "test@supermail.comic"
        , expected = Just ( "test", "supamail.cosmic", "test@supamail.cosmic" )
        }
    , Named
        { name = "Test what suggestions return for input ally@gmail.com with default suggest"
        , f = suggest
        , input = "ally@gmail.com"
        , expected = Nothing
        }
    , Named
        { name = "Test what suggestions return for input ally@aol.com with default suggest"
        , f = suggest
        , input = "ally@aol.com"
        , expected = Nothing
        }
    ]


runSuggestTest : String -> List SuggestCase -> Test
runSuggestTest testSetName data =
    let
        testIt : String -> (String -> Mailcheck) -> String -> Mailcheck -> Test
        testIt name suggestFunc input expected =
            test name
                (\() ->
                    Expect.equal
                        expected
                        (suggestFunc input)
                )

        testSet : List Test
        testSet =
            List.map
                (\caseData ->
                    case caseData of
                        Anon { f, input, expected } ->
                            testIt input f input expected

                        Named { name, f, input, expected } ->
                            testIt name f input expected
                )
                data
    in
    describe testSetName testSet
