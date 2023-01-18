library(examsMSB)

ef <- msb_eval()

ef$checkanswer(correct="1100", answer="1010")
ef$checkanswer(correct="1100", answer="1100")
ef$checkanswer(correct="1100", answer="1000")

ef$pointsum("1100","1010") # should 50%
ef$pointsum("1100","1100") # should 100%
ef$pointsum("1100","1000") # should 75%
ef$pointsum("1100","0001") # should 25%
