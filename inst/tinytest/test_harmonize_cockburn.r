## -------->>  [[file:../../harmonizer.src.org::*Combined Cockburn Procedures][Combined Cockburn Procedures:2]]
expect_equal(data.table(name = c("MÄKARÖNI ETÖ FKÜSNÖ Ltd"
                               , "M S Lab CÖ. <a href=lsdldf> <br> <\\a>"
                               , "MSLab Co."
                               , "MSLaeb Comp."
                               , "MSLab Comp. Ltd."
                               , "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝŸ") |> rep(20)
                      , foo = "Coffee" ) |>
             harmonize_cockburn(quite = TRUE)
           , structure(list(name = c("MAEKAROENI ETOE FKUESNOE", "MS LAB COE A HREF=LSDLDF BR A", 
                                     "MSLAB", "MSLAEB COMP", "MSLAB COMP", "AAAAAEAAECEEEEIIIIDNOOOOOEOUUUUEYŸ") |> rep(20)
                          , foo = "Coffee" |> rep(120)), row.names = c(NA, -120L), class = c("data.table", 
                                                                                             "data.frame")))






expect_equal(data.table(name = c("MÄKARÖNI ETÖ FKÜSNÖ Ltd"
                               , "M S Lab CÖ. <a href=lsdldf> <br> <\\a>"
                               , "MSLab Co."
                               , "MSLaeb Comp."
                               , "MSLab Comp. Ltd."
                               , "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝŸ") |> rep(20)
                      , foo = "Coffee" ) |>
             harmonize_cockburn(detect_legal_form = TRUE, quite = TRUE)
           , structure(list(name = c("MAEKAROENI ETOE FKUESNOE", "MS LAB COE A HREF=LSDLDF BR A", 
                                     "MSLAB", "MSLAEB COMP", "MSLAB COMP", "AAAAAEAAECEEEEIIIIDNOOOOOEOUUUUEYŸ") |> rep(20)
                          , foo = "Coffee" |> rep(120)
                          , entity_type = c(rep("firm", 5), NA) |>
                                rep(20)
                            ), row.names = c(NA, -120L), class = c("data.table", 
                                                                   "data.frame")))




expect_equal(data.table(name = c("MÄKARÖNI ETÖ FKÜSNÖ Ltd"
                               , "M S Lab CÖ. <a href=lsdldf> <br> <\\a>"
                               , "MSLab Co."
                               , "MSLaeb Comp."
                               , "MSLab Comp. Ltd."
                               , "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝŸ") |> rep(20)
                      , foo = "Coffee" ) |>
             harmonize_cockburn(return_x_before_common_words_removal = TRUE
                              , quite = TRUE)
           , structure(list(name = c("MAEKAROENI ETOE FKUESNOE", "MS LAB COE A HREF=LSDLDF BR A", 
                                     "MSLAB", "MSLAEB COMP", "MSLAB COMP", "AAAAAEAAECEEEEIIIIDNOOOOOEOUUUUEYŸ") |> rep(20)
                          , foo = "Coffee" |> rep(120)
                     , std_name_copy = c("  MAEKAROENI ETOE FKUESNOE LTD  ", "  MS LAB COE A HREF=LSDLDF BR A  ", 
                                         "  MSLAB CO  ", "  MSLAEB COMP  ", "  MSLAB COMP LTD  ", "  AAAAAEAAECEEEEIIIIDNOOOOOEOUUUUEYŸ  ") |>
                           rep(20))
                     , row.names = c(NA, -120L), class = c("data.table", 
                                                           "data.frame")))







## test progress (but it is so sloww...)
expect_equal(data.table(name = c("MÄKARÖNI ETÖ FKÜSNÖ Ltd"
                               , "M S Lab CÖ. <a href=lsdldf> <br> <\\a>"
                               , "MSLab Co."
                               , "MSLaeb Comp."
                               , "MSLab Comp. Ltd."
                               , "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝŸ") |> rep(20)
                      , foo = "Coffee" ) |>
             harmonize_cockburn(nrows_min_to_show_progress = 100
                              , detect_legal_form = TRUE
                              , quite = TRUE)
           , structure(list(name = c("MAEKAROENI ETOE FKUESNOE", "MS LAB COE A HREF=LSDLDF BR A", 
                                     "MSLAB", "MSLAEB COMP", "MSLAB COMP", "AAAAAEAAECEEEEIIIIDNOOOOOEOUUUUEYŸ") |> rep(20)
                          , foo = "Coffee" |> rep(120)
                          , entity_type = c(rep("firm", 5), NA) |>
                                rep(20)
                            ), row.names = c(NA, -120L), class = c("data.table", 
                                                                   "data.frame")))
## --------<<  Combined Cockburn Procedures:2 ends here


