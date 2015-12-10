module TestRunner where

import String
import Graphics.Element exposing (Element)

import ElmTest exposing (..)

import MailcheckTest
import StringDistanceTest


main : Element
main =
    elementRunner
      ( suite "Element Test Runner Tests"
        [ MailcheckTest.tests
        , StringDistanceTest.tests
        ]
      )
