## -------->>  [[file:../../nstandr.src.org::*Universities][Universities:2]]
expect_equal(c(" TILBURG UNIVERSTIY "
             , " VU UNIVERSTITAET "
             , "LEGALY REPRESENTED BY STAS") |>
             cockburn_detect_univ()
           , structure(list(x = c(" TILBURG UNIVERSTIY ", " VU UNIVERSTITAET ", 
                                  "LEGALY REPRESENTED BY STAS"), x_entity_type = c("univ", "univ", 
                                                                                 NA)), row.names = c(NA, -3L), class = c("data.table", "data.frame"
                                                                                                                         )))



expect_equal(c(" SUPERVISORS OF THE TILBURG UNIVERSTIY "
             , " VU UNIVERSTITAET "
             , "LEGALY REPRESENTED BY STAS") |>
             cockburn_replace_univ()
           , c(" TILBURG UNIVERSTIY ", " VU UNIVERSTITAET ", "LEGALY REPRESENTED BY STAS"
               ))
## --------<<  Universities:2 ends here


