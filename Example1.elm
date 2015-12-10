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
