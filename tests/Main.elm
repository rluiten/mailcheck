port module Main exposing (..)

import Test.Runner.Node exposing (run, TestProgram)
import Json.Encode exposing (Value)

import MailcheckTest


main : TestProgram
main =
    run emit MailcheckTest.tests


port emit : ( String, Value ) -> Cmd msg
