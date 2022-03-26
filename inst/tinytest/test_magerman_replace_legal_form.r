## -------->>  [[file:../../nstandr.src.org::*Detect and replace legal forms][Detect and replace legal forms:2]]
expect_equal(
    magerman_detect_legal_form_end(c("lksdjf MFG. CO, INC"
                                   , "MSlab Co."
                                   , "IBM Corp."
                                   , "MSlab Co. GMBH & CO.KG lalal"
                                   , "KABUSHIKI KAISHA MSlab Co. ") |>
                                   toupper())
  , structure(list(x = c("LKSDJF MFG. CO, INC", "MSLAB CO.", "IBM CORP."
                        , "MSLAB CO. GMBH & CO.KG LALAL", "KABUSHIKI KAISHA MSLAB CO. ")
                 , x_legal_form = c("INCORPORATED", NA, NA, NA, NA))
            , row.names = c(NA, -5L), class = c("data.table", "data.frame"))
)

expect_equal(c("lksdjf MFG. CO, INC"
             , "MSlab Co."
             , "IBM Corp."
             , "MSlab Co. GMBH & CO.KG lalal"
             , "KABUSHIKI KAISHA MSlab Co. ") |>
             toupper() |>
             magerman_replace_legal_form_end(output_placement = "append_to_col")
 , structure(list(x = c("LKSDJF MFG. CO, INC", "MSLAB CO.", "IBM CORP.", "MSLAB CO. GMBH & CO.KG LALAL", "KABUSHIKI KAISHA MSLAB CO. "), std_x = c("LKSDJF MANUFACTURING COMPANY", "MSLAB COMPANY", "IBM CORPORATION", "MSLAB CO. GMBH & CO.KG LALAL", "KABUSHIKI KAISHA MSLAB CO. ")), row.names = c(NA, -5L), class = c("data.table", "data.frame")))


expect_equal(magerman_detect_legal_form_end(c("lksdjf MFG. CO, INC"
                                            , "MSlab Co."
                                            , "IBM Corp."
                                            , "MSlab Co. GMBH & CO.KG lalal"
                                            , "KABUSHIKI KAISHA MSlab Co. ") |>
                                            toupper()
                                          , patterns_codes = TRUE
                                          , no_match_code = FALSE
                                          , return_only_codes = TRUE)
           , c(TRUE, TRUE, TRUE, FALSE, FALSE))

expect_equal(
    magerman_detect_legal_form_middle(c("lksdjf MFG. CO, INC"
                                               , "MSlab Co."
                                               , "IBM Corp."
                                               , "MSlab Co. GMBH & CO.KG lalal"
                                               , "KABUSHIKI KAISHA MSlab Co. ") |>
                                      toupper())
           , structure(list(x = c("LKSDJF MFG. CO, INC", "MSLAB CO.", "IBM CORP."
                                  ,"MSLAB CO. GMBH & CO.KG LALAL", "KABUSHIKI KAISHA MSLAB CO. "
                                   ), x_legal_form = c(NA, NA, NA, "GMBH", NA))
                     , row.names = c(NA, -5L), class = c("data.table", "data.frame")))

expect_equal(magerman_detect_legal_form_beginning(c("lksdjf MFG. CO, INC"
                                                  , "MSlab Co."
                                                  , "IBM Corp."
                                                  , "MSlab Co. GMBH & CO.KG lalal"
                                                  , "KABUSHIKI KAISHA MSlab Co. ") |>
                                                  toupper()), structure(list(x = c("LKSDJF MFG. CO, INC", "MSLAB CO.", "IBM CORP.", 
                                   "MSLAB CO. GMBH & CO.KG LALAL", "KABUSHIKI KAISHA MSLAB CO. "
                                   ), x_legal_form = c(NA, NA, NA, NA, "KAISHA")), row.names = c(NA, 
                                                                                             -5L), class = c("data.table", "data.frame")))
## --------<<  Detect and replace legal forms:2 ends here


