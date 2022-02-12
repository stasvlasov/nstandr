## -------->>  [[file:../../harmonizer.src.org::*Firms (Corporates)][Firms (Corporates):2]]
expect_equal(c(" DR VLASOV & BROTHER "
          ,  "MSlab & C"
          , " S.VLASOV PHD "
          , "LEGALY REPRESENTED BY STAS") |>
          cockburn_detect_corp()
        , structure(list(x = c(" DR VLASOV & BROTHER ", "MSlab & C", " S.VLASOV PHD ", 
                               "LEGALY REPRESENTED BY STAS"), x_entity_type = c("firm", NA, NA, 
                                                                              NA)), row.names = c(NA, -4L), class = c("data.table", "data.frame"
                                                                                                                      )))
## --------<<  Firms (Corporates):2 ends here


