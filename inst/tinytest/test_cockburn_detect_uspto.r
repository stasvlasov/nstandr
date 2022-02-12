## -------->>  [[file:../../harmonizer.src.org::*USPTO special][USPTO special:2]]
expect_equal(c("WESTINGHOUSE, |.?^&*@ ELEC-CONN.  "
             , "GRACE-CONN. (W EN R) & CO - Ã²Ã¢ÃªÃ®Ã©  PUBLIC LIMITED "
             , "Bechara;John") |> 
             cockburn_remove_uspto()
           , c("WESTINGHOUSE, |.?^&*@ ELEC  ", "GRACE (W EN R) & CO - Ã²Ã¢ÃªÃ®Ã©  PUBLIC LIMITED ", 
               "Bechara ; John"))



expect_equal(c("WESTINGHOUSE, |.?^&*@ ELEC-CONN.  "
             , "GRACE-CONN. (W EN R) & CO - Ã²Ã¢ÃªÃ®Ã©  PUBLIC LIMITED "
             , "Bechara;John") |> 
             cockburn_detect_uspto()
           , structure(list(x = c("WESTINGHOUSE, |.?^&*@ ELEC-CONN.  ", "GRACE-CONN. (W EN R) & CO - Ã²Ã¢ÃªÃ®Ã©  PUBLIC LIMITED ", 
                                  "Bechara;John"), x_entity_type = c(NA, NA, "indiv")), row.names = c(NA, 
                                                                                                    -3L), class = c("data.table", "data.frame")))
## --------<<  USPTO special:2 ends here


