## -------->>  [[file:../../nstandr.src.org::*standardize_toascii][standardize_toascii:1]]
## This fails depending on platform (Windows) on R release.

## expect_equal( c("FAÇILE"
##               , "fa\xE7ile"
##               , "c\u00b5c\u00b5ber") |>
##               data.table::data.table("coffee") |>
##               standardize_toascii(detect_encoding = TRUE)

## , structure(list(V1 = c("FAAILE", "facile", "cucuber"), V2 = c("coffee", 
## "coffee", "coffee")), row.names = c(NA, -3L), class = c("data.table", 
## "data.frame")))
## --------<<  standardize_toascii:1 ends here


