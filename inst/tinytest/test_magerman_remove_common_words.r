## -------->>  [[file:../../nstandr.src.org::*Common Words][Common Words:2]]
expect_equal(c("lksdjf MFG. GMBH CO,; INC"
             , "MSlab Co."
             , "IBM Corp."
             , " MSlab Co. GMBH & CO.KG lalal  "
             , "KABUSHIKI KAISHA MSlab Co.") |>
             toupper() |>
             magerman_remove_legal_form_and_clean() |>
             magerman_remove_common_words()
           , c("LKSDJF MFG. GMBH CO", "MSLAB ", "IBM ", "MSLAB &LALAL", "KABUSHIKI KAISHA MSLAB "))
## --------<<  Common Words:2 ends here


