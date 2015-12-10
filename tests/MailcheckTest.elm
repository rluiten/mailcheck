module MailcheckTest where

import String

import ElmTest exposing (..)
import Mailcheck exposing
    ( Mailcheck(Suggestion, NoSuggestion)
    , findClosestDomain
    , splitEmail
    , encodeEmail
    , suggest
    , suggestWith)


domains = ["google.com", "gmail.com", "emaildomain.com", "comcast.net", "facebook.com", "msn.com", "gmx.de"]
topLevelDomains = ["co.uk", "com", "org", "info", "fr"]
secondLevelDomains = ["yahoo", "hotmail", "mail", "live", "outlook", "gmx"]


tests : Test
tests =
    suite "Mailcheck tests"
      [ encodeEmailTests
      , splitEmailTests
      , findClosestDomainTests
      , suggestTests
      ]


findClosestDomainTests : Test
findClosestDomainTests =
    suite "findClosestDomain"
      [ --runFcdTest "returns the most similar domain" domainsData
      --, runFcdTest "returns the most similar second-level domain" secondLevelDomainsData
      --, runFcdTest "returns the most similar top-level domain" topLevelDomainsData
      --,
      runFcdTest "limit search and return Nothing as needed" findLimitSearchRange
      ]


-- List expect, input, domains
type alias ExpectedFindClosestDomainExamples = List (Maybe String, String, List String)


domainsData : ExpectedFindClosestDomainExamples
domainsData =
    [ (Just "google.com", "google.com", domains)
    , (Just "gmail.com", "gmail.com", domains)
    , (Just "emaildomain.com", "emaildoman.com", domains)
    , (Just "msn.com", "gmsn.com", domains)
    , (Just "gmail.com", "gmaik.com", domains)
    ]


secondLevelDomainsData : ExpectedFindClosestDomainExamples
secondLevelDomainsData =
    [ (Just "hotmail", "hotmial", secondLevelDomains)
    , (Just "yahoo", "tahoo", secondLevelDomains)
    , (Just "live", "livr", secondLevelDomains)
    , (Just "outlook", "outllok", secondLevelDomains)
    ]


topLevelDomainsData : ExpectedFindClosestDomainExamples
topLevelDomainsData =
    [ (Just "com", "cmo", topLevelDomains)
    , (Just "org", "ogr", topLevelDomains)
    , (Just "info", "ifno", topLevelDomains)
    , (Just "co.uk", "com.uk", topLevelDomains)
    ]


limitSearchStrings =
  [ "xxxxxx"
  , "ovenmat"
  ]


findLimitSearchRange : ExpectedFindClosestDomainExamples
findLimitSearchRange =
    [ (Just "ovenmat", "oven", limitSearchStrings)
    , (Nothing, "cat", limitSearchStrings)
    , (Nothing, "ve", limitSearchStrings)
    , (Just "ovenmat", "ven", limitSearchStrings)
    , (Just "ovenmat", "nma", limitSearchStrings)
    , (Just "ovenmat", "xenma", limitSearchStrings)
    , (Nothing, "mate", limitSearchStrings)
    ]


runFcdTest : String -> ExpectedFindClosestDomainExamples -> Test
runFcdTest name data =
    let
      tests =
        List.map
          (\(expect, input, domains) ->
            test input -- input String doubles as test name
              (assertEqual
                expect
                (findClosestDomain input domains)
              )
          ) data
    in
      suite name tests


splitEmailTests : Test
splitEmailTests =
    suite "splitEmail"
      [ runSplitEmailTest "returns a hash of the address, the domain, and the top-level domain" splitEmailData
      , runSplitEmailTest "splits RFC compliant emails" splitRfcCompliantData
      , runSplitEmailTest "returns Nothing for email addresses that are not RFC compliant" splitNonRfcCompliantData
      , runSplitEmailTest "trims spaces from the start and end of the string" splitTrimsSpacesData
      ]


-- List (email, (split result))
type alias ExpectedSplitEmailExamples = List (String, Maybe(String, String, String, String))


splitEmailData : ExpectedSplitEmailExamples
splitEmailData =
    [ ( "test@example.com"
      , Just ("test", "example.com", "example", "com"))
    , ( "test@example.co.uk"
      , Just ("test", "example.co.uk", "example", "co.uk"))
    , ( "test@mail.randomsmallcompany.co.uk"
      , Just ("test", "mail.randomsmallcompany.co.uk", "mail", "randomsmallcompany.co.uk"))
    ]


splitRfcCompliantData : ExpectedSplitEmailExamples
splitRfcCompliantData =
    [ ( "\"foo@bar\"@example.com"
      , Just ("\"foo@bar\"", "example.com", "example", "com"))
    , ( "containsnumbers1234567890@example.com"
      , Just ("containsnumbers1234567890", "example.com", "example", "com"))
    , ( "contains+symbol@example.com"
      , Just ("contains+symbol", "example.com", "example", "com"))
    , ( "contains-symbol@example.com"
      , Just ("contains-symbol", "example.com", "example", "com"))
    , ( "contains.symbol@domain.contains.symbol"
      , Just ("contains.symbol", "domain.contains.symbol", "domain", "contains.symbol"))

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
      , Just ("\"contains.and symbols\"", "example.com", "example", "com"))

    , ( "\"contains.and.@.symbols.com\"@example.com"
      , Just ("\"contains.and.@.symbols.com\"", "example.com", "example", "com"))

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
       , Just ("\"()<>[]:;@,\\\"!#$%&'*+-/=?^_`{}|     ~       ?            ^_`{}|~.a\""
              , "allthesymbols.com"
              , "allthesymbols"
              , "com")
       )

    , ( "postbox@com"
      , Just ("postbox", "com", "", "com"))
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
      , Just ("postbox", "com", "", "com"))
    , ( "postbox@com "
      , Just ("postbox", "com", "", "com"))
    ]


runSplitEmailTest : String -> ExpectedSplitEmailExamples -> Test
runSplitEmailTest name data =
    let
      tests =
        List.map
          (\(email, expect) ->
            test name -- name String doubles as test name
              (assertEqual
                expect
                (splitEmail email)
              )
          ) data
    in
      suite name tests


encodeEmailTests =
    suite "encodeEmail tests"
      [ runEncodeEmailTest "example cases" encodeEmailExamples ]


-- List (input, output)
type alias ExpectedEncodeEmailExamples = List (String, String)


encodeEmailExamples : ExpectedEncodeEmailExamples
encodeEmailExamples =
  [ ( "hello@test.com", "hello@test.com" )
  , ( "hello+more@test.com", "hello+more@test.com" )
  , ( "hello+more(comment)@test.com", "hello+more(comment)@test.com" )
  , ( "(comment)hello+more@test.com", "(comment)hello+more@test.com" )
  , ( "a !#$%&'*+-/=?^_`{|}~\"@test.com", "a !#$%&'*+-/=?^_`{|}~\"@test.com")
  , ( "<script>alert(\"a\")</xscript>@emaildomain.con"
    , "%3Cscript%3Ealert(\"a\")%3C/xscript%3E@emaildomain.con" )
  --, ( "<script>alert(\"a\")</script>@emaildomain.con", "cxxx" ) -- Compiler broken </script>
  ]


runEncodeEmailTest : String -> ExpectedEncodeEmailExamples -> Test
runEncodeEmailTest name data =
    let
      tests =
        List.map
          (\(input, expect) ->
            defaultTest
              (assertEqual
                expect
                (encodeEmail input)
              )
          ) data
    in
      suite name tests




suggestTests : Test
suggestTests = runSuggestTest "suggest cases" suggestionData


type SuggestCase
    = AnonSuggestCase (String -> Mailcheck, String, Mailcheck)
    | NamedSuggestCase (String, String -> Mailcheck, String, Mailcheck)


type alias ExpectedSuggestExamples = List SuggestCase


suggestionData : ExpectedSuggestExamples
suggestionData =
  [ AnonSuggestCase
      ( suggest
      , "test@gmail.co"
      , Suggestion ("test", "gmail.com", "test@gmail.com") )
  , NamedSuggestCase
      ( "takes in an array of specified domains"
      , suggestWith domains [] []
      , "test@emaildomain.con"
      , Suggestion ("test", "emaildomain.com", "test@emaildomain.com")
      )
  , NamedSuggestCase
      ( "domain not close to any domain in default domain list test@emaildomain.con but tld found"
      , suggest
      , "test@emaildomain.con"
      , Suggestion ("test", "emaildomain.co.nz", "test@emaildomain.co.nz")
      )
  , NamedSuggestCase
      ( "custom domain list is near to misspelled domain"
      , suggestWith domains [] []
      , "test@xmaildomain.con"
      , Suggestion ("test", "emaildomain.com", "test@emaildomain.com")
      )
  , AnonSuggestCase
      ( suggest
      , "contact@kicksend.com"
      , NoSuggestion )
  , NamedSuggestCase
      ( "no suggestion if incomplete email provided"
      , suggest
      , "contact"
      , NoSuggestion
      )
  , AnonSuggestCase
      ( suggest
      , "test@gmailc.om"
      , Suggestion ("test", "gmail.com", "test@gmail.com")
      )
  , AnonSuggestCase
      ( suggestWith domains [] []
      , "test@emaildomain.co"
      , Suggestion ("test", "emaildomain.com", "test@emaildomain.com")
      )
  , AnonSuggestCase
      ( suggestWith domains [] []
      , "test@gnail.con"
      , Suggestion ("test", "gmail.com", "test@gmail.com")
      )
  , AnonSuggestCase
      ( suggestWith domains [] []
      , "test@gsnail.con"
      , NoSuggestion
      )
  , AnonSuggestCase
      ( suggestWith domains [] []
      , "test@GNAIL.con"
      , Suggestion ("test", "gmail.com", "test@gmail.com")
      )
  , AnonSuggestCase
      ( suggestWith domains [] []
      , "test@#gmail.com"
      , Suggestion ("test", "gmail.com", "test@gmail.com")
      )
  , AnonSuggestCase
      ( suggestWith domains [] []
      , "test@comcast.nry"
      , Suggestion ("test", "comcast.net", "test@comcast.net")
      )

  , AnonSuggestCase
      ( suggestWith domains secondLevelDomains topLevelDomains
      , "test@homail.con"
      , Suggestion ("test", "hotmail.com", "test@hotmail.com")
      )
  , AnonSuggestCase
      ( suggestWith domains secondLevelDomains topLevelDomains
      , "test@hotmail.co"
      , Suggestion ("test", "hotmail.com", "test@hotmail.com")
      )
  , AnonSuggestCase
      ( suggestWith domains secondLevelDomains topLevelDomains
      , "test@yajoo.com"
      , Suggestion ("test", "yahoo.com", "test@yahoo.com")
      )
  , AnonSuggestCase
      ( suggestWith domains secondLevelDomains topLevelDomains
      , "test@randomsmallcompany.cmo"
      , Suggestion ("test", "randomsmallcompany.com", "test@randomsmallcompany.com")
      )

  , NamedSuggestCase
      ( "empty email address produces NoSuggestion"
      , suggestWith domains secondLevelDomains topLevelDomains
      , ""
      , NoSuggestion
      )
  , NamedSuggestCase
      ( "missing email address domain produces NoSuggestion"
      , suggestWith domains secondLevelDomains topLevelDomains
      , "test@"
      , NoSuggestion
      )
  , NamedSuggestCase
      ( "not @ or domain in email produces NoSuggestion"
      , suggestWith domains secondLevelDomains topLevelDomains
      , "test@"
      , NoSuggestion
      )

  , NamedSuggestCase
      ( "this test is for illustrative purposes as the splitEmail function should return a better representation of the true top-level domain in the case of an email address with subdomains. mailcheck will be unable to return a suggestion in the case of this email address"
      , suggestWith domains secondLevelDomains topLevelDomains
      , "test@mail.randomsmallcompany.cmo"
      , NoSuggestion
      )

  , NamedSuggestCase
      ( "will not offer a suggestion that itself leads to another suggestion"
      , suggestWith domains secondLevelDomains topLevelDomains
      , "test@yahooo.cmo"
      , Suggestion ("test", "yahoo.com", "test@yahoo.com")
      )

  , NamedSuggestCase
      ( "will not offer suggestions for valid 2ld-tld combinations"
      , suggestWith domains secondLevelDomains topLevelDomains
      , "test@yahoo.co.uk"
      , NoSuggestion
      )

  , NamedSuggestCase
      ( "will not offer suggestions for valid 2ld-tld even if theres a close fully-specified domain"
      , suggestWith domains secondLevelDomains topLevelDomains
      , "test@gmx.fr"
      , NoSuggestion
      )

  , NamedSuggestCase
      ( "an example used in readme"
      , suggestWith ["yohomail.com"] [ "supamail" ] [ "cosmic" ]
      , "test@ohomail.co"
      , Suggestion ("test", "yohomail.com", "test@yohomail.com")
      )
  , NamedSuggestCase
      ( "an example used in readme"
      , suggestWith ["yohomail.com"] [ "supamail" ] [ "cosmic" ]
      , "test@fakedomain.comic"
      , Suggestion ("test", "fakedomain.cosmic", "test@fakedomain.cosmic")
      )
  , NamedSuggestCase
      ( "an example used in readme"
      , suggestWith ["yohomail.com"] [ "supamail" ] [ "cosmic" ]
      , "test@supermail.comic"
      , Suggestion ("test", "supamail.cosmic", "test@supamail.cosmic")
      )
  ]

runSuggestTest : String -> ExpectedSuggestExamples -> Test
runSuggestTest name data =
    let
      tests =
        List.map
          (\caseData->
            let
              (name, suggest, input, expect) =
                case caseData of
                  AnonSuggestCase (suggest', input', expect') ->
                    (input', suggest', input', expect')
                  NamedSuggestCase (name', suggest', input', expect') ->
                    (name', suggest', input', expect')
            in
              test name
                (assertEqual
                  expect
                  (suggest input)
                )
          ) data
    in
      suite name tests
