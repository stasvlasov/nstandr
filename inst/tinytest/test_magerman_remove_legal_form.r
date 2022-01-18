## -------->>  [[file:../../harmonizer.src.org::*Detect and replace legal forms][Detect and replace legal forms:4]]
expect_equal(
    c("lksdjf MFG. GMBH CO, INC"
             , "MSlab Co."
             , "IBM Corp."
             , "MSlab Co. GMBH & CO.KG lalal"
             , "KABUSHIKI KAISHA MSlab Co. ") |>
             toupper() |>
             magerman_detect_legal_form()
  , structure(list(V1 = c("LKSDJF MFG. GMBH CO, INC", "MSLAB CO."
                                 , "IBM CORP.", "MSLAB CO. GMBH & CO.KG LALAL", "KABUSHIKI KAISHA MSLAB CO. ")
                          , V1_coded = c("INCORPORATED", NA, NA, "GMBH", "KAISHA")), row.names = c(NA,-5L)
                     , class = c("data.table", "data.frame")))



## debug(get_dots)

## undebug(get_dots)



    c("lksdjf MFG. GMBH CO, INC"
             , "MSlab Co."
             , "IBM Corp."
             , "MSlab Co. GMBH & CO.KG lalal"
             , "KABUSHIKI KAISHA MSlab Co. ") |>
             toupper() |>
             magerman_remove_legal_form(output = "append_to_col")




#                              V1                V1_harmonized
# 1:     LKSDJF MFG. GMBH COMPANY     LKSDJF MFG. GMBH COMPANY
# 2:                MSLAB COMPANY                MSLAB COMPANY
# 3:              IBM CORPORATION              IBM CORPORATION
# 4: MSLAB CO. GMBH & CO.KG LALAL MSLAB CO. GMBH & CO.KG LALAL
# 5:  KABUSHIKI KAISHA MSLAB CO.                    MSLAB CO. 



#                              V1                V1_harmonized
# 1:     LKSDJF MFG. GMBH COMPANY     LKSDJF MFG. GMBH COMPANY
# 2:                MSLAB COMPANY                MSLAB COMPANY
# 3:              IBM CORPORATION              IBM CORPORATION
# 4: MSLAB CO. GMBH & CO.KG LALAL MSLAB CO. GMBH & CO.KG LALAL
# 5:  KABUSHIKI KAISHA MSLAB CO.                    MSLAB CO. 



#                              V1                V1_harmonized
# 1:     LKSDJF MFG. GMBH CO, INC     LKSDJF MFG. GMBH COMPANY
# 2:                    MSLAB CO.                MSLAB COMPANY
# 3:                    IBM CORP.              IBM CORPORATION
# 4: MSLAB CO. GMBH & CO.KG LALAL MSLAB CO. GMBH & CO.KG LALAL
# 5:  KABUSHIKI KAISHA MSLAB CO.   KABUSHIKI KAISHA MSLAB CO. 



#                              V1                V1_harmonized
# 1:     LKSDJF MFG. GMBH CO, INC     LKSDJF MFG. GMBH COMPANY
# 2:                    MSLAB CO.                MSLAB COMPANY
# 3:                    IBM CORP.              IBM CORPORATION
# 4: MSLAB CO. GMBH & CO.KG LALAL MSLAB CO. GMBH & CO.KG LALAL
# 5:  KABUSHIKI KAISHA MSLAB CO.   KABUSHIKI KAISHA MSLAB CO. 






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


