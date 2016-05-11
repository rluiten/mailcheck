module TestRunner exposing (..)

import String
import ElmTest exposing (..)

import MailcheckTest

main =
  runSuite
    ( suite "Program Test Runner Tests"
      [ MailcheckTest.tests
      ]
    )
