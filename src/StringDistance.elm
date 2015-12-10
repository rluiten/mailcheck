module StringDistance
    ( sift3Distance
    , lcs
    , lcsLimit
    ) where


{-| A library to calculate the string distance between two strings.
This is ported from the code in https://github.com/mailcheck/mailcheck/blob/master/src/mailcheck.js.
Which refers back to http://siderite.blogspot.com.au/2007/04/super-fast-and-accurate-string-distance.html as
source of algorithm

Functions
@docs sift3Distance
@docs lcs
@docs lcsLimit

-}

import String

{-| Calculate sift3 string distance between candidate strings.

```elm
    sift3Distance "" "abc" == 3.0
    sift3Distance "ab" "" == 2.0
    sift3Distance "abc" "abc" == 0
    sift3Distance "abc" "ab"  == 0.5
```

-}
sift3Distance : String -> String -> Float
sift3Distance s1 s2 =
    let
      s1Len = String.length s1
      s2Len = String.length s2
      lcs' : List Char -> List Char -> List Char
      lcs' = lcsLimit 5
    in
      if s1Len == 0 then
        toFloat s2Len

      else if s2Len == 0 then
        toFloat s1Len

      else
        let common = lcs' (String.toList s1) (String.toList s2)
            --_ = Debug.log("sift3Distance") (s1, s2, common, List.length common)
        in  (toFloat (s1Len + s2Len) / 2) - toFloat (List.length common)


{-| Longest Common Subsequence

This is a simple implementation and would benefit from memoization if
performance is a problem. It does not limit look ahead
which can be very costly see lcsLimit for a limited look ahead version.

Warning this gets very slow very quickly with increases in list lengths even
17 character strings can cause things to bog down.

This implementation is based on http://rosettacode.org/wiki/Longest_common_subsequence#Haskell

```elm
    lcs ["a", "b", "c"] ["b", "c", "d"] == ["b", "c"]
```

 -}
lcs : List Char -> List Char -> List Char
lcs xs' ys' =
    case (xs', ys') of
      ((x::xs), (y::ys)) ->
        if x == y then
          x :: lcs xs ys
        else
          maxl (lcs xs' ys) (lcs xs ys')
      _ -> []


{-| Return function which returns lcs with limited look ahead.

Warning maxLookAhead quickly makes the returned function costly stay
below 8 if you want responsiveness.

```elm
    lcsLimit 5 ["a", "b", "c"] ["b", "c", "d"] == ["b", "c"]
```

-}
lcsLimit : Int -> List Char -> List Char -> List Char
lcsLimit maxLookAhead = lcsLimit' 0 maxLookAhead


{-| Implementation of Longest Common Subsequence with look ahead limit.

This is a simple implementation and would benefit from memoization.
-}
lcsLimit' : Int -> Int -> List Char -> List Char -> List Char
lcsLimit' offset maxLookAhead xs' ys' =
    if offset > maxLookAhead then
      []

    else
      case (xs', ys') of
        ((x::xs), (y::ys)) ->
          if x == y then
            x :: lcsLimit' 0 maxLookAhead xs ys
          else
            maxl
              (lcsLimit' (offset + 1) maxLookAhead xs' ys)
              (lcsLimit' (offset + 1) maxLookAhead xs ys')
        _ -> []


{-| Return the List which is longer -}
maxl xs ys = if List.length xs > List.length ys then xs else ys
