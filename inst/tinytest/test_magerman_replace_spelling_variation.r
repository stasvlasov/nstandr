## -------->>  [[file:../../nstandr.src.org::*Spelling Variation][Spelling Variation:2]]
expect_equal(c("CHEMICALS SYSTEMEN MSlab Ltd."
             , "ELECTRONICS SYSTEMES MSlab Co.") |>
             magerman_replace_spelling_variation()
           , c("CHEMICAL SYSTEM MSlab Ltd.", "ELECTRONIC SYSTEM MSlab Co."))
## --------<<  Spelling Variation:2 ends here


