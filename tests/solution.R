#library(examsMSB)

item_list <- list.files(here::here("tests/items/"),full.names = TRUE)[1:2]

examsMSB::exams2nops(file=item_list,
                     n=1,
                     dir="tests/exam",points=1, showpoints=TRUE,
                     samepage=TRUE, blank = 0, intro="",
                     title="Test Klausur", reglength=12,
                     duplex=FALSE, language="de",
                     name="testklausur")

examsMSB::exams2nops(file=item_list,
n=1,
dir="tests/exam",points=1, showpoints=TRUE,
samepage=TRUE, blank = 0, intro="",
title="Test Klausur", reglength=12,
duplex=FALSE, language="de",
name="testklausur", solution = TRUE)
