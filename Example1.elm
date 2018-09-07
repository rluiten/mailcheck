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
