## -------->>  [[file:../../nstandr.src.org::*Standard Name][Standard Name:2]]
expect_equal(c("WESTINGHOUSE, |.?^&*@ ELEC  "
             , "GRACE (W EN R) & CO - Ã²Ã¢ÃªÃ®Ã©  PUBLIC LIMITED "
             , "GRACE (W/R) & CO LTD ") |>
             cockburn_replace_standard_names(output_placement = "append_to_x")
           , structure(list(x = c("WESTINGHOUSE, |.?^&*@ ELEC  ", "GRACE (W EN R) & CO - Ã²Ã¢ÃªÃ®Ã©  PUBLIC LIMITED ", 
                                  "GRACE (W/R) & CO LTD "), std_x = c("WESTINGHOUSE, |.?^&*@ ELEC  ", 
                                                                      "GRACE (W EN R) & CO - Ã²Ã¢ÃªÃ®Ã©  PUBLIC LTD ", "GRACE (W/R) & CO LTD "
                                                                      )), row.names = c(NA, -3L), class = c("data.table", "data.frame")))
## --------<<  Standard Name:2 ends here


