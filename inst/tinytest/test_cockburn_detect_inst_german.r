## -------->>  [[file:../../harmonizer.src.org::*German Non-profit institutes][German Non-profit institutes:2]]
expect_equal(c(" EINGETRAGENER VEREIN UNIV "
             , " BERLIN EINGETRAGENER VEREIN "
             , " STIFTUNG ") |>
             cockburn_detect_inst_german()
           , structure(list(x = c(" EINGETRAGENER VEREIN UNIV ", " BERLIN EINGETRAGENER VEREIN ", 
                                  " STIFTUNG "), x_entity_type = c("inst", NA, NA)), row.names = c(NA, 
                                                                                                 -3L), class = c("data.table", "data.frame")))
## --------<<  German Non-profit institutes:2 ends here


