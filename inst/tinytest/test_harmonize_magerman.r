## -------->>  [[file:../../harmonizer.src.org::*Combined Magerman Procedures][Combined Magerman Procedures:2]]
## Test with return magerman_procedures in harmonize_magerman
harmonize_magerman()


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
             harmonize_magerman(nrows_min_to_show_progress = 50
                              , show_progress = TRUE
                              , detect_legal_form = FALSE)
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
             harmonize_magerman(nrows_min_to_show_progress = 50
                              , show_progress = TRUE
                              , append_output_copy_before_common_words_removal = TRUE
                              , detect_legal_form = FALSE)
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
             harmonize_magerman(col = 2
                              , nrows_min_to_show_progress = 50
                              , show_progress = TRUE
                              , detect_legal_form = FALSE)
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
             harmonize_magerman(nrows_min_to_show_progress = 50
                              , show_progress = TRUE
                              , detect_legal_form = TRUE)
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
## --------<<  Combined Magerman Procedures:2 ends here


