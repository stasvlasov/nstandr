## -------->>  [[file:../../harmonizer.src.org::*harmonize_detect_enc][harmonize_detect_enc:2]]
expect_equal(data.table(data = c("FAÇILE"
                               , "fa\xE7ile"
                               , "c\u00b5c\u00b5ber")
                      , coffee = "Yes, please!") |>
             harmonize_detect_enc(return_only_codes = TRUE)
           , c("UTF-8", "ISO-8859-9", "UTF-8"))


## TODO col naming

expect_equal(c("FAÇILE"
             , "fa\xE7ile"
             , "c\u00b5c\u00b5ber") |>
             harmonize_detect_enc()
           , structure(list(x = c("FAÇILE", "fa\xe7ile", "cµcµber"), x_encoding = c("UTF-8", 
                                                                                    "ISO-8859-9", "UTF-8")), row.names = c(NA, -3L), class = c("data.table", 
                                                                                                                                               "data.frame")))



expect_equal(data.table(data = c("FAÇILE"
                               , "fa\xE7ile"
                               , "c\u00b5c\u00b5ber")
                      , coffee = "Yes, please!") |>
             harmonize_detect_enc(output_codes_col_name = "{col_name}_lala")
           , structure(list(data = c("FAÇILE", "fa\xe7ile", "cµcµber"), 
                            coffee = c("Yes, please!", "Yes, please!", "Yes, please!"
                                       ), data_lala = c("UTF-8", "ISO-8859-9", "UTF-8")), row.names = c(NA, 
                                                                                                        -3L), class = c("data.table", "data.frame")))
## --------<<  harmonize_detect_enc:2 ends here


