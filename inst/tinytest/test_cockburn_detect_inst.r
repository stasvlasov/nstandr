## -------->>  [[file:../../harmonizer.src.org::*Non-profit institutes][Non-profit institutes:2]]
expect_equal(c(" DR VLASOV "
             , " S.VLASOV PHD "
             , " FUNDATION LEGALY REPRESENTED BY STAS") |>
             cockburn_detect_inst()
           , structure(list(x = c(" DR VLASOV ", " S.VLASOV PHD ", " FUNDATION LEGALY REPRESENTED BY STAS"
                                  ), x_entity_type = c(NA, NA, "inst")), row.names = c(NA, -3L), class = c("data.table", 
                                                                                                         "data.frame")))
## --------<<  Non-profit institutes:2 ends here


