## -------->>  [[file:../../nstandr.src.org::*Stem Name][Stem Name:2]]
expect_equal(c("WESTINGHOUSE, |.?^&*@ ELEC  "
             , "GRACE (W EN R) & CO - Ã²Ã¢ÃªÃ®Ã©  PUBLIC LIMITED "
             , "GRACE (W/R) & CO LTD ") |>
             cockburn_remove_standard_names()
           , c("WESTINGHOUSE, |.?^&*@ ELEC  ", "GRACE (W EN R) - Ã²Ã¢ÃªÃ®Ã©  PUBLIC LIMITED ", "GRACE (W/R) "))
## --------<<  Stem Name:2 ends here


