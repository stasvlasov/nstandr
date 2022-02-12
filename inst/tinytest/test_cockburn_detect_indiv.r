## -------->>  [[file:../../harmonizer.src.org::*Individuals][Individuals:2]]
expect_equal(c(" DR VLASOV "
             , " S.VLASOV PHD "
             , "LEGALY REPRESENTED BY STAS") |>
             cockburn_detect_indiv()
           , structure(list(x = c(" DR VLASOV ", " S.VLASOV PHD ", "LEGALY REPRESENTED BY STAS"
                                  ), x_entity_type = c("indiv", "indiv", NA)), row.names = c(NA, 
                                                                                           -3L), class = c("data.table", "data.frame")))



expect_equal(c(" DR VLASOV "
             , " S.VLASOV PHD "
             , " STANICA LEGALY REPRESENTED BY STAS"
             , " DR VLASOV & BROTHER "
             , "MSlab & C"
             , "LEGALY REPRESENTED BY STAS"
             , " REPUBLIC LEGALY REPRESENTED BY STAS"
             , " TILBURG UNIVERSTIY "
             , " VU UNIVERSTITAET "
             , " FUNDATION LEGALY REPRESENTED BY STAS") |>
             cockburn_detect_indiv() |>
             cockburn_detect_govt() |>
             cockburn_detect_indiv()
, structure(list(x = c(" DR VLASOV ", " S.VLASOV PHD ", " STANICA LEGALY REPRESENTED BY STAS", 
" DR VLASOV & BROTHER ", "MSlab & C", "LEGALY REPRESENTED BY STAS", 
" REPUBLIC LEGALY REPRESENTED BY STAS", " TILBURG UNIVERSTIY ", 
" VU UNIVERSTITAET ", " FUNDATION LEGALY REPRESENTED BY STAS"
), x_entity_type = list(c("indiv", "indiv"), c("indiv", "indiv"
), c("indiv", "indiv"), c("indiv", "indiv"), character(0), character(0), 
    c("indiv", "govt", "indiv"), character(0), character(0), 
    c("indiv", "indiv"))), row.names = c(NA, -10L), class = c("data.table", 
"data.frame")))
## --------<<  Individuals:2 ends here


