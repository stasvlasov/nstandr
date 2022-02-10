## -------->>  [[file:../../harmonizer.src.org::*harmonize_toascii][harmonize_toascii:2]]
expect_equal( c("FAÇILE"
              , "fa\xE7ile"
              , "c\u00b5c\u00b5ber") |>
              data.table("coffee") |>
              harmonize_toascii(detect_encoding = TRUE)
           , structure(list(V1 = c("FACILE", "facile", "cucuber"), V2 = c("coffee", 
                                                                          "coffee", "coffee")), row.names = c(NA, -3L), class = c("data.table", 
                                                                                                                                  "data.frame")))

 expect_equal(c("FAÇILE"
              , "fa\xE7ile"
              , "c\u00b5c\u00b5ber") |>
              data.table("coffee") |>
              harmonize_toascii()
            , structure(list(V1 = c("FACILE", "fa<e7>ile", "cucuber"), V2 = c("coffee", 
                                                                              "coffee", "coffee")), row.names = c(NA, -3L), class = c("data.table", 
                                                                                                                                      "data.frame")))
## --------<<  harmonize_toascii:2 ends here


