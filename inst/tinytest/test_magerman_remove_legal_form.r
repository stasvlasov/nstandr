## -------->>  [[file:../../harmonizer.src.org::*Detect and replace legal forms][Detect and replace legal forms:4]]
expect_equal(

    c("lksdjf MFG. GMBH CO, INC"
    , "MSlab Co."
    , "IBM Corp."
    , "MSlab Co. GMBH & CO.KG lalal"
    , "KABUSHIKI KAISHA MSlab Co. ") |>
    toupper() |>
    magerman_detect_legal_form()


#                              V1     V1_coded
# 1:     LKSDJF MFG. GMBH CO, INC INCORPORATED
# 2:                    MSLAB CO.         <NA>
# 3:                    IBM CORP.         <NA>
# 4: MSLAB CO. GMBH & CO.KG LALAL         GMBH
# 5:  KABUSHIKI KAISHA MSLAB CO.          <NA>



#                              V1     V1_coded
# 1:     LKSDJF MFG. GMBH CO, INC INCORPORATED
# 2:                    MSLAB CO.         <NA>
# 3:                    IBM CORP.         <NA>
# 4: MSLAB CO. GMBH & CO.KG LALAL         GMBH
# 5:  KABUSHIKI KAISHA MSLAB CO.        KAISHA



#                              V1     V1_coded
# 1:     LKSDJF MFG. GMBH CO, INC INCORPORATED
# 2:                    MSLAB CO.         <NA>
# 3:                    IBM CORP.         <NA>
# 4: MSLAB CO. GMBH & CO.KG LALAL         GMBH
# 5:  KABUSHIKI KAISHA MSLAB CO.        KAISHA



## without print(x)
#                              V1     V1_coded
# 1:     LKSDJF MFG. GMBH CO, INC INCORPORATED
# 2:                    MSLAB CO.         <NA>
# 3:                    IBM CORP.         <NA>
# 4: MSLAB CO. GMBH & CO.KG LALAL         GMBH
# 5:  KABUSHIKI KAISHA MSLAB CO.          <NA>


## with print(x)
#                              V1     V1_coded
# 1:     LKSDJF MFG. GMBH CO, INC INCORPORATED
# 2:                    MSLAB CO.         <NA>
# 3:                    IBM CORP.         <NA>
# 4: MSLAB CO. GMBH & CO.KG LALAL         GMBH
# 5:  KABUSHIKI KAISHA MSLAB CO.        KAISHA










  , structure(list(V1 = c("LKSDJF MFG. GMBH CO, INC", "MSLAB CO."
                                 , "IBM CORP.", "MSLAB CO. GMBH & CO.KG LALAL", "KABUSHIKI KAISHA MSLAB CO. ")
                          , V1_coded = c("INCORPORATED", NA, NA, "GMBH", "KAISHA")), row.names = c(NA,-5L)
                     , class = c("data.table", "data.frame")))



## debug(get_dots)

## undebug(get_dots)


## testing pecular (but intendent) behaviour of magerman_remove_legal_form
## in the last string the middle part should not be replaced because of legal form at the end
    c("lksdjf MFG. GMBH CO, INC"
    , "MSlab Co."
    , "IBM Corp."
    , "MSlab Co. GMBH & CO.KG lalal"
    , "KABUSHIKI KAISHA MSlab Co. "
    , "KABUSHIKI KAISHA MSlab Co.") |>
             toupper() |>
             magerman_remove_legal_form(output = "append_to_col")







expect_equal(
    c("lksdjf MFG. GMBH CO,; INC"
             , "MSlab Co."
             , "IBM Corp."
             , " MSlab Co. GMBH & CO.KG lalal  "
             , "KABUSHIKI KAISHA MSlab Co.") |>
             toupper() |>
             magerman_remove_legal_form_and_clean()
, c("LKSDJF MFG. GMBH CO,;", "MSLAB COMPANY", "IBM CORPORATION", 
" MSLAB CO. & COMPANY LALAL  ", "KABUSHIKI KAISHA MSLAB COMPANY")
)


  data.table(c("lksdjf MFG. GMBH CO,; INC"
             , "MSlab Co."
             , "IBM Corp."
             , " MSlab Co. GMBH & CO.KG lalal  "
             , "KABUSHIKI KAISHA MSlab Co.") |> toupper()
           , somevar = c(1,2,3,4,5)) |>
      magerman_remove_legal_form_and_clean(output = "append_to_col")

#                                V1                  V1_harmonized somevar
# 1:          LKSDJF MFG. GMBH CO,;          LKSDJF MFG. GMBH CO,;       1
# 2:                  MSLAB COMPANY                  MSLAB COMPANY       2
# 3:                IBM CORPORATION                IBM CORPORATION       3
# 4:    MSLAB CO. & COMPANY LALAL      MSLAB CO. & COMPANY LALAL         4
# 5: KABUSHIKI KAISHA MSLAB COMPANY KABUSHIKI KAISHA MSLAB COMPANY       5



replace_patterns(c("lksdjf MFG. GMBH CO,; INC"
             , "MSlab Co."
             , "IBM Corp."
             , " MSlab Co. GMBH & CO.KG lalal  "
             , "KABUSHIKI KAISHA MSlab Co.")
             , magerman_patterns_legal_form_middle
             , output = "prepend_to_x")[]




, structure(list(V1 = c("LKSDJF MFG. GMBH CO,;", "MSLAB COMPANY", 
"IBM CORPORATION", " MSLAB CO. & COMPANY LALAL  ", " MSLAB COMPANY"
), somevar = c(1, 2, 3, 4, 5)), row.names = c(NA, -5L), class = c("data.table", 
"data.frame"))
## --------<<  Detect and replace legal forms:4 ends here


