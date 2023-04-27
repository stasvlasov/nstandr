## -------->>  [[file:../../nstandr.src.org::*Hospitals][Hospitals:2]]
expect_equal(c(" DR VLASOV "
             , " S.VLASOV PHD "
             , " STANICA LEGALY REPRESENTED BY STAS") |>
             cockburn_detect_hosp()
           , structure(list(x = c(" DR VLASOV ", " S.VLASOV PHD ", " STANICA LEGALY REPRESENTED BY STAS"
                                  ), x_entity_type = c(NA, NA, "hosp")), row.names = c(NA, -3L), class = c("data.table", 
                                                                                                         "data.frame")))
## --------<<  Hospitals:2 ends here


