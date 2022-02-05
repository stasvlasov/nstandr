## -------->>  [[file:../../harmonizer.src.org::*Detect and replace legal forms][Detect and replace legal forms:3]]
expect_equal(
    c("lksdjf MFG. GMBH CO, INC"
    , "MSlab Co."
    , "IBM Corp."
    , "MSlab Co. GMBH & CO.KG lalal CO."
    , "KABUSHIKI KAISHA MSlab Co.") |>
    toupper() |>
    magerman_detect_legal_form()
  , structure(list(x = c("LKSDJF MFG. GMBH CO, INC", "MSLAB CO.", "IBM CORP.", "MSLAB CO. GMBH & CO.KG LALAL CO.", "KABUSHIKI KAISHA MSLAB CO."  ), x_legal_form = c("INCORPORATED", NA, NA, "GMBH", "KAISHA")), row.names = c(NA, -5L), class = c("data.table", "data.frame")))




## testing pecular (but intendent) behaviour of magerman_remove_legal_form
## in the last string the middle part should not be replaced because of legal form at the end
expect_equal(c("lksdjf MFG. GMBH CO, INC"
             , "MSlab Co."
             , "IBM Corp."
             , "MSlab Co. GMBH & CO.KG lalal"
             , "KABUSHIKI KAISHA MSlab Co. "
             , "KABUSHIKI KAISHA MSlab Co.") |>
             toupper() |>
             magerman_remove_legal_form()
           , c("LKSDJF MFG. GMBH COMPANY", "MSLAB COMPANY", "IBM CORPORATION", "MSLAB CO. & COMPANY LALAL", " MSLAB CO. ", "KABUSHIKI KAISHA MSLAB COMPANY"))



expect_equal(
    c("lksdjf MFG. GMBH CO,; INC"
    , "MSlab Co."
    , "IBM Corp."
    , " MSlab Co. GMBH & CO.KG lalal  "
    , "KABUSHIKI KAISHA MSlab Co.") |>
    toupper() |>
    magerman_remove_legal_form_and_clean()
, c("LKSDJF MFG. GMBH CO", "MSLAB COMPANY", "IBM CORPORATION", 
"MSLAB CO. & COMPANY LALAL", "KABUSHIKI KAISHA MSLAB COMPANY"))


expect_equal(
    data.table(c("lksdjf MFG. GMBH CO,; INC"
                        , "MSlab Co."
                        , "IBM Corp."
                        , " MSlab Co. GMBH & CO.KG lalal  "
                        , "KABUSHIKI KAISHA MSlab Co.") |> toupper()
                      , somevar = c(1,2,3,4,5)) |>
             magerman_remove_legal_form_and_clean(output_placement = "append_to_col")
, structure(list(V1 = c("LKSDJF MFG. GMBH CO,; INC", "MSLAB CO.", 
"IBM CORP.", " MSLAB CO. GMBH & CO.KG LALAL  ", "KABUSHIKI KAISHA MSLAB CO."
), std_V1 = c("LKSDJF MFG. GMBH CO", "MSLAB COMPANY", "IBM CORPORATION", 
"MSLAB CO. & COMPANY LALAL", "KABUSHIKI KAISHA MSLAB COMPANY"
), somevar = c(1, 2, 3, 4, 5)), row.names = c(NA, -5L), class = c("data.table", 
"data.frame"))
)

expect_equal(
    data.table(c("lksdjf MFG. GMBH CO,; INC"
                        , "MSlab Co."
                        , "IBM Corp."
                        , " MSlab Co. GMBH & CO.KG lalal  "
                        , "KABUSHIKI KAISHA MSlab Co.") |> toupper()
                      , somevar = c(1,2,3,4,5)) |>
    magerman_remove_legal_form_and_clean(output_placement = "prepend_to_x")
, structure(list(std_V1 = c("LKSDJF MFG. GMBH CO", "MSLAB COMPANY", 
"IBM CORPORATION", "MSLAB CO. & COMPANY LALAL", "KABUSHIKI KAISHA MSLAB COMPANY"
), V1 = c("LKSDJF MFG. GMBH CO,; INC", "MSLAB CO.", "IBM CORP.", 
" MSLAB CO. GMBH & CO.KG LALAL  ", "KABUSHIKI KAISHA MSLAB CO."
), somevar = c(1, 2, 3, 4, 5)), row.names = c(NA, -5L), class = c("data.table", 
"data.frame"))
)



expect_equal(
    c("lksdjf MFG. GMBH CO,; INC"
             , "MSlab Co."
             , "IBM Corp."
             , " MSlab Co. GMBH & CO.KG lalal  "
             , "KABUSHIKI KAISHA MSlab Co. ") |>
             toupper() |>
             magerman_replace_legal_form_end(output_placement = "prepend_to_x")
, structure(list(std_x = c("LKSDJF MFG. GMBH CO,;", "MSLAB COMPANY", 
"IBM CORPORATION", " MSLAB CO. GMBH & CO.KG LALAL  ", "KABUSHIKI KAISHA MSLAB CO. "
), x = c("LKSDJF MFG. GMBH CO,; INC", "MSLAB CO.", "IBM CORP.", 
" MSLAB CO. GMBH & CO.KG LALAL  ", "KABUSHIKI KAISHA MSLAB CO. "
)), row.names = c(NA, -5L), class = c("data.table", "data.frame"
)))
## --------<<  Detect and replace legal forms:3 ends here


