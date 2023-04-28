## -------->>  [[file:../../nstandr.src.org::*standardize_detect_enc][standardize_detect_enc:2]]
## This fails depending on platform (Windows) on R release.

## expect_equal(data.table::data.table(data = c("FAÇILE"
##                                , "fa\xE7ile"
##                                , "c\u00b5c\u00b5ber")
##                       , coffee = "Yes, please!") |>
##              standardize_detect_enc(return_only_codes = TRUE)
##            , c("WINDOWS-1254", "ISO-8859-9", "UTF-8"))


## ## TODO col naming

## expect_equal(c("FAÇILE"
##              , "fa\xE7ile"
##              , "c\u00b5c\u00b5ber") |>
##              standardize_detect_enc()
## , structure(list(x = c("FAÇILE", "fa\xe7ile", "cµcµber"), x_encoding = c("WINDOWS-1254", 
## "ISO-8859-9", "UTF-8")), row.names = c(NA, -3L), class = c("data.table", 
## "data.frame")))




## expect_equal(data.table::data.table(data = c("FAÇILE"
##                                            , "fa\xE7ile"
##                                            , "c\u00b5c\u00b5ber")
##                                   , coffee = "Yes, please!") |>
##              standardize_detect_enc(output_codes_col_name = "{col_name}_lala")
##            , structure(list(data = c("FAÇILE", "fa\xe7ile", "cµcµber"), 
##                             coffee = c("Yes, please!", "Yes, please!", "Yes, please!"
##                                        ), data_lala = c("WINDOWS-1254", "ISO-8859-9", "UTF-8")), row.names = c(NA, 
##                                                                                                                -3L), class = c("data.table", "data.frame")))
## --------<<  standardize_detect_enc:2 ends here


