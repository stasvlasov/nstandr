## -------->>  [[file:../../nstandr.src.org::*Combined Magerman Procedures][Combined Magerman Procedures:2]]
## Test with return magerman_procedures in standardize_magerman
standardize_magerman()


## Test
expect_equal(data.table(name = c("MÄKARÖNI ETÖ FKÜSNÖ Ltd"
                               , "MSLab CÖ. <a href=lsdldf> <br> <\\a>"
                               , "MSLab Co."
                               , "MSLaeb Comp."
                               , "MSLab Comp. Ltd."
                               , "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝŸ") |>
                            rep(20) |>
                            toupper()
                      , foo = "I love coffee" ) |>
             standardize_magerman(nrows_min_to_show_progress = 50
                              , show_progress = TRUE
                              , detect_legal_form = FALSE
                              , quite = TRUE)
           , data.table(name = c("MAEKAEROENIETOEFKUESNOE"
                               , "MSLAEBAEHREFLSDLDFAE"
                               , "MSLAB"
                               , "MSLAEBCOMP"
                               , "MSLABCOMP"
                               , "AEAEAEAEAEAEAECEEEEIIIINOEOEOEOEOEUEUEUEUEYY") |>
                            rep(20)
                      , foo = "I love coffee"))

## works now but weird naming of append_output_copy
expect_equal(data.table(name = c("MÄKARÖNI ETÖ FKÜSNÖ Ltd"
                               , "MSLab CÖ. <a href=lsdldf> <br> <\\a>"
                               , "MSLab Co."
                               , "MSLaeb Comp."
                               , "MSLab Comp. Ltd."
                               , "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝŸ") |>
                            rep(10) |>
                            toupper()
                      , foo = "I love coffee" ) |>
             standardize_magerman(nrows_min_to_show_progress = 50
                              , show_progress = TRUE
                              , append_output_copy_before_common_words_removal = TRUE
                              , detect_legal_form = FALSE
                              , quite = TRUE)
           , data.table(name = c("MAEKAEROENIETOEFKUESNOE"
                               , "MSLAEBAEHREFLSDLDFAE"
                               , "MSLAB"
                               , "MSLAEBCOMP"
                               , "MSLABCOMP"
                               , "AEAEAEAEAEAEAECEEEEIIIINOEOEOEOEOEUEUEUEUEYY") |>
                            rep(10)
                      , foo = "I love coffee"
                      , name_before_common_words_removal = c("MAKARONI ETO FKUSNO"
                                                           , "MSLAB CO. A HREFLSDLDF A"
                                                           , "MSLAB COMPANY"
                                                           , "MSLAEB COMP."
                                                           , "MSLAB COMP."
                                                           , "AAAAAAAECEEEEIIIINOOOOOUUUUYY") |>
                            rep(10)))

## test differe columns
expect_equal(data.table(foo = "I love coffee"
                      , name = c("MÄKARÖNI ETÖ FKÜSNÖ Ltd"
                               , "MSLab CÖ. <a href=lsdldf> <br> <\\a>"
                               , "MSLab Co."
                               , "MSLaeb Comp."
                               , "MSLab Comp. Ltd."
                               , "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝŸ") |>
                            rep(10) |>
                            toupper()) |>
             standardize_magerman(col = 2
                              , nrows_min_to_show_progress = 50
                              , show_progress = TRUE
                              , detect_legal_form = FALSE
                              , quite = TRUE)
           , data.table(foo = "I love coffee"
                      , name = c("MAEKAEROENIETOEFKUESNOE"
                               , "MSLAEBAEHREFLSDLDFAE"
                               , "MSLAB"
                               , "MSLAEBCOMP"
                               , "MSLABCOMP"
                               , "AEAEAEAEAEAEAECEEEEIIIINOEOEOEOEOEUEUEUEUEYY") |>
                            rep(10)))


## issue with name match in detect_patterns with return_only_codes
expect_equal(data.table(name = c("MÄKARÖNI ETÖ FKÜSNÖ Ltd"
                               , "MSLab CÖ. <a href=lsdldf> <br> <\\a>"
                               , "MSLab Co."
                               , "MSLaeb Comp."
                               , "MSLab Comp. Ltd."
                               , "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝŸ") |>
                            rep(10) |>
                            toupper()
                      , foo = "I love coffee" ) |>
             standardize_magerman(nrows_min_to_show_progress = 50
                              , show_progress = TRUE
                              , detect_legal_form = TRUE
                              , quite = TRUE)
           , data.table(name = c("MAEKAEROENIETOEFKUESNOE"
                               , "MSLAEBAEHREFLSDLDFAE"
                               , "MSLAB"
                               , "MSLAEBCOMP"
                               , "MSLABCOMP"
                               , "AEAEAEAEAEAEAECEEEEIIIINOEOEOEOEOEUEUEUEUEYY") |>
                            rep(10)
                      , foo = "I love coffee"
                      , name_legal_form = c("LIMITED", NA, NA, NA, "LIMITED", NA)|>
                            rep(10)))


## test standartization itself
expect_equal(c("SGS-THOMSON MICROELECTRONICS"
             , "S.G.S. THOMSON MICROELECTRONICS S.R.L."
             , "S.G.S. THOMSON MICROELECTRONICS, S.R.L."
             , "S.G.S.-THOMSON MICROELECTRONICS S.R.L."
             , "SGS - THOMSON MICROELECTRONICS S.A."
             , "SGS - THOMSON MICROELECTRONICS S.R.L."
             , "SGS - THOMSON MICROELECTRONICS, INC."
             , "SGS - THOMSON MICROELECTRONICS, S.R.L."
             , "SGS THOMSON MICROELECTRONICS S.A."
             , "SGS THOMSON MICROELECTRONICS S.R.L."
             , "SGS THOMSON MICROELECTRONICS SA"
             , "SGS THOMSON MICROELECTRONICS SRL"
             , "SGS THOMSON MICROELECTRONICS, INC."
             , "SGS THOMSON MICROELECTRONICS, S.A."
             , "SGS- THOMSON MICROELECTRONICS, S.A."
             , "SGS THOMSON MICROELECTRONICS, S.R.L."
             , "SGS- THOMSON MICROELECTRONICS<BR>(PTE) LTD."
             , "SGS THOMSON-MICROELECTRONICS SA"
             , "SGS-THOMSON MICROELECTRONIC S.A."
             , "SGS-THOMSON MICROELECTRONICS"
             , "SGS-THOMSON MICROELECTRONICS GMBH"
             , "SGS-THOMSON MICROELECTRONICS INC."
             , "SGS-THOMSON MICROELECTRONICS LIMITED"
             , "SGS-THOMSON MICROELECTRONICS LTD."
             , "SGS-THOMSON MICROELECTRONICS PTE LTD"
             , "SGS-THOMSON MICROELECTRONICS PTE LTD."
             , "SGS-THOMSON MICROELECTRONICS PTE. LIMITED"
             , "SGS-THOMSON MICROELECTRONICS PTE. LTD."
             , "SGS-THOMSON MICROELECTRONICS S. R. L."
             , "SGS-THOMSON MICROELECTRONICS S.A"
             , "SGS-THOMSON MICROELECTRONICS S.A."
             , "SGS-THOMSON MICROELECTRONICS S.P.A."
             , "SGS-THOMSON MICROELECTRONICS S.R. L."
             , "SGS-THOMSON MICROELECTRONICS S.R.L"
             , "SGS-THOMSON MICROELECTRONICS S.R.L."
             , "SGS--THOMSON MICROELECTRONICS S.R.L."
             , "SGS-THOMSON MICROELECTRONICS SA"
             , "SGS-THOMSON MICROELECTRONICS SPA"
             , "SGS-THOMSON MICROELECTRONICS SRL"
             , "SGS-THOMSON MICROELECTRONICS SRL."
             , "SGS-THOMSON MICROELECTRONICS, GMBH"
             , "SGS-THOMSON MICROELECTRONICS, INC"
             , "SGS-THOMSON MICROELECTRONICS, INC."
             , "SGS-THOMSON MICROELECTRONICS, LTD."
             , "SGS-THOMSON MICROELECTRONICS, PTE LTD."
             , "SGS-THOMSON MICROELECTRONICS, S.A."
             , "SGS-THOMSON MICROELECTRONICS, S.R.L."
             , "SGS-THOMSON MICROELECTRONICS, S.RL"
             , "SGS-THOMSON MICROELECTRONICS, SA"
             , "SGS-THOMSON MICROELECTRONICS, SA."
             , "SGS-THOMSON MICROELECTRONICS, SRL"
             , "SGS-THOMSON MICROELECTRONICS,S.R.L.") |>
             standardize_magerman(quite = TRUE)
           , rep("SGSTHOMSONMICROELECTRONIC", 52))


expect_equal(c("E.I. DU PONT DE NEMOURS & COMPANY"
             , "E I DU PONT DE NEMOURS AND COMPANY"
             , "E I DUPONT DE NEMOURS AND COMPANY"
             , "E I. DU PONT DE NEMOURS AND COMPANY"
             , "E. .I DU PONT DE NEMOURS AND COMPANY"
             , "E. I DU PONT DE NEMOURS AND COMPANY"
             , "E. I DU PONT DE NEMOURS AND COMPANY."
             , "E. I. DU PONT DE NEMOURS"
             , "E. I. DU PONT DE NEMOURS & CO"
             , "E. I. DU PONT DE NEMOURS & CO."
             , "E. I. DU PONT DE NEMOURS & CO. (INC.)"
             , "E. I. DU PONT DE NEMOURS & CO., INC."
             , "E. I. DU PONT DE NEMOURS & COMPANY"
             , "E. I. DU PONT DE NEMOURS AND CO."
             , "E. I. DU PONT DE NEMOURS AND CO., INC."
             , "E. I. DU PONT DE NEMOURS AND COMPANY"
             , "E. I. DU PONT DE NEMOURS AND COMPANY, INC."
             , "E. I. DU PONT DE NEMOURS AND COMPANY."
             , "E. I. DU PONT DE NEMOURS CO."
             , "E. I. DU PONT DE NEMOURS CO., INC."
             , "E. I. DU PONT DE NEMOURS COMPANY"
             , "E. I. DU PONT DE NEMOURS COMPANY, INC."
             , "E. I. DUPONT DE NEMOURS & CO."
             , "E. I. DUPONT DE NEMOURS & COMPANY"
             , "E. I. DUPONT DE NEMOURS AND COMPANY"
             , "E. I. DUPONT DENEMOURS & COMPANY"
             , "E. I. DUPONT DENEMOURS AND COMPANY"
             , "E. I.DU PONT DE NEMOURS AND COMPANY"
             , "E.I . DU PONT DE NEMOURS AND COMPANY"
             , "E.I. DU PONT DE NEMOURS & CO."
             , "E.I. DU PONT DE NEMOURS & CO., INC."
             , "E.I. DU PONT DE NEMOURS & COMPANY"
             , "E.I. DU PONT DE NEMOURS & COMPANY, INC"
             , "E.I. DU PONT DE NEMOURS &AMP; CO. (INC.)"
             , "E.I. DU PONT DE NEMOURS &AMP; COMPANY"
             , "E.I. DU PONT DE NEMOURS &AMP; COMPANY INC."
             , "E.I. DU PONT DE NEMOURS &AMP;"
               ## , "E.I. DU PONT DE NEMOURS &AMP; COMPAY, INC."
             , "E.I. DU PONT DE NEMOURS AND COMPANY"
             , "E.I. DU PONT DE NEMOURS<BR>AND CO."
             , "E.I. DU PONT DE NEMOURS<BR>AND COMPANY"
             , "E.I. DUPONT DE NEMOURS"
             , "E.I. DUPONT DE NEMOURS & CO."
             , "E.I. DUPONT DE NEMOURS & COMPANY"
             , "E.I. DUPONT DE NEMOURS AND CO."
             , "E.I. DUPONT DE NEMOURS AND COMPANY"
             , "E.I. DUPONT DE NEMOURS AND COMPANY, INC."
             , "E.I. DUPONT DENEMOURS & COMPANY"
             , "E.I. DUPONT DENEMOURS AND COMPANY"
             , "E.I.DU PONT DE NEMOURS AND COMPANY"
             , "EI DU PONT DE NEMOURS AND COMPANY"
             , "EI DUPONT DE NEMOURS AND COMPANY") |>
             standardize_magerman(quite = TRUE)
           , rep("EIDUPONTDENEMOURS", 51))

expect_equal(c("Chip &AMP; Dayle (lala) [0x2345] {abs} ops html <br>"
             , "&AMP;&OACUTE;&SECT; 02937lkjfas;ldjf  &UACUTE;&#8902;&BULL; sdlfkjhhhh ;laskdjf&EXCL;"
             , "&AMP;&OACUTE;&SECT; {UMLAUT OVER (E)} sdlfkjhhhh ;laskdjf&EXCL;"
             , "ŠŒŽšœžŸ¥µÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝßàáâãäåæçèéêëìíîïðñòóôõöøùúûüýÿ"
             , "LK \tD©𝍎 ၍\tF:'\";092834!@#$%^&*()_+-\n\t"
             , "  a   string with   many      douple    spaces      "
             , "\"\" Merry  \"Cristmas\" Love\"\""
             , "\"\"Merry  \"Cristmas\" Love\"\""
             , "MSLab Co.++"
             , "A sentence with .irregular punctuation ,like commas , and periods ."
             , "MSlab ,INC. ,LTD"
             , "CHEMICALS SYSTEMEN MSlab Ltd."
             , "MSLab CÖ.") |>
             standardize_magerman(quite = TRUE)
           , c("CHIPDAYLELALA0X2345ABSOPSHTML", "O02937LKJFASLDJFUSDLFKJHHHHLASKDJF", 
               "OESDLFKJHHHHLASKDJF", "YAEAEAEAEAEAEAECEEEEIIIINOEOEOEOEOEUEUEUEUEYAEAEAEAEAEAEAECEEEEIIIINOEOEOEOEOEUEUEUEUEYY", 
               "LKDF092834", "ASTRINGWITHMANYDOUPLESPACES", "MERRYCRISTMASLOVE", 
               "MERRYCRISTMASLOVE", "MSLAEB", "ASENTENCEWITHIRREGULARPUNCTUATIONLIKECOMMASANDPERIODS", 
               "MSLAEB", "CHEMICALSYSTEMMSLAB", "MSLAEB"))
## --------<<  Combined Magerman Procedures:2 ends here


