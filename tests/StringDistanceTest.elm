module StringDistanceTest where

import String

import ElmTest exposing (..)
import StringDistance exposing (sift3Distance)


tests : Test
tests =
    let testLcs s1 s2 =
          String.fromList (StringDistance.lcs (String.toList s1) (String.toList s2))
        testLcsLimit5 s1 s2 =
          String.fromList (StringDistance.lcsLimit 5 (String.toList s1) (String.toList s2))
    in
      suite "StringDistance tests"
        [ test "Trivial Test to ensure ElmTest is behaving" (assertEqual "a" "a")
        , suite "sift3Distance tests"
            [ test "Both strings empty returns 0" (assertEqual 0 (sift3Distance "" ""))
            , test "First string empty returns length of Second string" (assertEqual 3 (sift3Distance "" "abc"))
            , test "Second string empty returns length of First string" (assertEqual 2 (sift3Distance "ab" ""))
            , test "Two same strings" (assertEqual 0 (sift3Distance "abc" "abc"))
            , test "Two different strings \"abc\" \"ab\"" (assertEqual 0.5 (sift3Distance "abc" "ab"))
            , test "Two different strings \"helloworld\" \"world\"" (assertEqual 2.5 (sift3Distance "helloworld" "world"))

            -- These are to slow using lcs, fine with lcsLimit
            , test "1 char different start 17 chars long" (assertEqual 0.5 (sift3Distance "abcdefghijklmnopq" "bcdefghijklmnopq"))
            , test "2 char different start 17 chars long" (assertEqual 1.0 (sift3Distance "abcdefghijklmnopq" "cdefghijklmnopq"))
            , test "3 char different start 17 chars long" (assertEqual 1.5 (sift3Distance "abcdefghijklmnopq" "defghijklmnopq"))
            , test "4 char different start 17 chars long" (assertEqual 2.0 (sift3Distance "abcdefghijklmnopq" "efghijklmnopq"))
            , test "5 char different start 17 chars long" (assertEqual 2.5 (sift3Distance "abcdefghijklmnopq" "fghijklmnopq"))
            , test "6 char different start 17 chars long" (assertEqual 14 (sift3Distance "abcdefghijklmnopq" "ghijklmnopq"))

           -- this correlates with a test in Mailcheck for suggest to check string distance for its domains
            , test "check distance on this case" (assertEqual 7.5 (sift3Distance "kicksend.com" "emaildomain.com"))
            ]
        , suite "lcs tests"
            [ test
                "1 char different start \"abcdefgh\" \"bcdefgh\""
                (assertEqual "bcdefgh" (testLcs "abcdefgh" "bcdefgh"))
            , test
                "2 char different start\"abcdefgh\" \"cdefgh\""
                (assertEqual "cdefgh" (testLcs "abcdefgh" "cdefgh"))
            , test
                "3 char different start \"abcdefgh\" \"defgh\""
                (assertEqual "defgh" (testLcs "abcdefgh" "defgh"))
            , test
                "4 char different start \"abcdefgh\" \"efgh\""
                (assertEqual "efgh" (testLcs "abcdefgh" "efgh"))
            , test
                "5 char different start \"abcdefgh\" \"fgh\""
                (assertEqual "fgh" (testLcs "abcdefgh" "fgh"))
            ]
        , suite "lcsLimit tests"
            [ test
                "1 char different start \"abcdefgh\" \"bcdefgh\""
                (assertEqual "bcdefgh" (testLcsLimit5 "abcdefgh" "bcdefgh"))
            , test
                "2 char different start\"abcdefgh\" \"cdefgh\""
                (assertEqual "cdefgh" (testLcsLimit5 "abcdefgh" "cdefgh"))
            , test
                "3 char different start \"abcdefgh\" \"defgh\""
                (assertEqual "defgh" (testLcsLimit5 "abcdefgh" "defgh"))
            , test
                "4 char different start \"abcdefgh\" \"efgh\""
                (assertEqual "efgh" (testLcsLimit5 "abcdefgh" "efgh"))
            , test
                "5 char different start \"abcdefgh\" \"fgh\""
                (assertEqual "fgh" (testLcsLimit5 "abcdefgh" "fgh"))
            , test
                "6 char different start \"abcdefgh\" \"gh\""
                (assertEqual "" (testLcsLimit5 "abcdefgh" "gh"))
            , test
                "1 char different start longer string \"abcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQURSTUVWXYZ\" \"bcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQURSTUVWXYZ\""
                (assertEqual "bcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQURSTUVWXYZ"
                  (testLcsLimit5 "abcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQURSTUVWXYZ" "bcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQURSTUVWXYZ"))
            ]
        ]
