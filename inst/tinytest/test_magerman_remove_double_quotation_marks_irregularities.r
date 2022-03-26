## -------->>  [[file:../../nstandr.src.org::*magerman.remove.double.quotation.marks.*][magerman.remove.double.quotation.marks.*:2]]
expect_equal(c("\"\" Merry  \"Cristmas\" Love\"\""
             , "\"\"Merry \"\"Cristmas\"\" Love \"\"") |>
             magerman_remove_double_quotation_marks_irregularities()
           , c("\"\"Merry  \"Cristmas\" Love\"\""
             , "\"\"Merry \"\"Cristmas\"\" Love\"\""))
## --------<<  magerman.remove.double.quotation.marks.*:2 ends here


