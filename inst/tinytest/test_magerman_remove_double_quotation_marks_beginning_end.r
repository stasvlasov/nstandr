## -------->>  [[file:../../nstandr.src.org::*magerman.remove.double.quotation.marks.*][magerman.remove.double.quotation.marks.*:4]]
expect_equal(c("\"\"Merry  \"Cristmas\" Love\"\"" # delete quotes here
             , "\"\"Merry \"\"Cristmas\"\" Love\"\""  # do not delete here
               ) |>
             magerman_remove_double_quotation_marks_beginning_end()
           , c("Merry  \"Cristmas\" Love"
             , "\"\"Merry \"\"Cristmas\"\" Love\"\""))
## --------<<  magerman.remove.double.quotation.marks.*:4 ends here


