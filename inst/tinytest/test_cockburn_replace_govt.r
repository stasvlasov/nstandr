## -------->>  [[file:../../harmonizer.src.org::*Government][Government:2]]
expect_equal(c(" DR VLASOV "
             , " S.VLASOV PHD "
             , " REPUBLIC LEGALY REPRESENTED BY STAS") |>
             cockburn_detect_govt()
           , structure(list(x = c(" DR VLASOV ", " S.VLASOV PHD ", " REPUBLIC LEGALY REPRESENTED BY STAS"
                                  ), x_entity_type = c(NA, NA, "govt")), row.names = c(NA, -3L), class = c("data.table", 
                                                                                                         "data.frame")))

expect_equal(" VERY IMPORTANT SEC OF THE DEPT OF  " |>
             cockburn_replace_govt()
           , " VERY IMPORTANT DEPT OF  ")
## --------<<  Government:2 ends here


