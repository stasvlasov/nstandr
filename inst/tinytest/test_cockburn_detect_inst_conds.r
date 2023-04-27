## -------->>  [[file:../../nstandr.src.org::*Complex conditions][Complex conditions:2]]
expect_equal(data.table(org = c(" EVIL FOUND OF BIG CORP "
                              , " EVIL FOUND OF BIG CORP "
                              , " INT INST OF MAGIC"
                              , " COUNCIL OF PARANORMAL RES & DEV "
                              , " COUNCIL OF GROWN UP KIDS ")
                      , org_entity_type = list(c("univ", "gov"), NA, "univ", NA, "gov")) |>
             cockburn_detect_inst_conds_2()
           , structure(list(org = c(" EVIL FOUND OF BIG CORP ", " EVIL FOUND OF BIG CORP ", 
                                    " INT INST OF MAGIC", " COUNCIL OF PARANORMAL RES & DEV ", " COUNCIL OF GROWN UP KIDS "
                                    ), org_entity_type = list(c("univ", "gov"), "inst", "univ", NA_character_, 
                                                          "gov")), row.names = c(NA, -5L), class = c("data.table", 
                                                                                                     "data.frame")))




expect_equal(data.table(name = c("MÄKARÖNI ETÖ FKÜSNÖ Ltd"
                               , "MSLab CÖ. <a href=lsdldf> <br> <\\a>"
                               , "MSLab Co."
                               , "MSLaeb Comp."
                               , " INST OF PARANORMAL RES & DEV "
                               , "MSLab Comp. Ltd."
                               , "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝŸ")
                      , foo = "I love coffee") |>
             cockburn_detect_inst_conds(merge_existing_codes = "append_to_existing")
           , structure(list(name = c("MÄKARÖNI ETÖ FKÜSNÖ Ltd", "MSLab CÖ. <a href=lsdldf> <br> <\\a>", 
                                     "MSLab Co.", "MSLaeb Comp.", " INST OF PARANORMAL RES & DEV ", 
                                     "MSLab Comp. Ltd.", "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝŸ"
                                     ), foo = c("I love coffee", "I love coffee", "I love coffee", 
                                                "I love coffee", "I love coffee", "I love coffee", "I love coffee"
                                                ), name_entity_type = c(NA, NA, NA, NA, "inst", NA, NA)), row.names = c(NA, 
                                                                                                                   -7L), class = c("data.table", "data.frame")))



expect_equal(data.table(org = c(" DR VLASOV "
                              , " S.VLASOV PHD "
                              , " STANICA LEGALY REPRESENTED BY STAS INST "
                              , " INST DR VLASOV & BROTHER "
                              , "MSlab & C"
                              , "LEGALY REPRESENTED BY STAS"
                              , " REPUBLIC LEGALY REPRESENTED BY STAS"
                              , " TILBURG UNIVERSTIY "
                              , " VU UNIVERSTITAET "
                              , " FUNDATION LEGALY REPRESENTED BY STAS")
                      , org_entity_type = list(c("univ", "gov"), NA, "univ", NA, "gov")) |>
             cockburn_detect_inst_conds()
           , structure(list(org = c(" DR VLASOV ", " S.VLASOV PHD ", " STANICA LEGALY REPRESENTED BY STAS INST ", 
                                    " INST DR VLASOV & BROTHER ", "MSlab & C", "LEGALY REPRESENTED BY STAS", 
                                    " REPUBLIC LEGALY REPRESENTED BY STAS", " TILBURG UNIVERSTIY ", 
                                    " VU UNIVERSTITAET ", " FUNDATION LEGALY REPRESENTED BY STAS"
                                    ), org_entity_type = list(c("univ", "gov"), NA_character_, "univ", 
                                                          "inst", "gov", c("univ", "gov"), NA_character_, "univ", NA_character_, 
                                                          "gov")), row.names = c(NA, -10L), class = c("data.table", 
                                                                                                      "data.frame")))
## --------<<  Complex conditions:2 ends here


