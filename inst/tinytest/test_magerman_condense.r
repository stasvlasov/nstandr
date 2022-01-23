## -------->>  [[file:../../harmonizer.src.org::*Condensing][Condensing:2]]
expect_equal(c("lksdjf MFG. GMBH CO,; INC"
             , "MSlab Co."
             , "IBM Corp."
             , " MSlab Co. GMBH & CO.KG lalal  "
             , "KABUSHIKI KAISHA MSlab Co.") |>
             magerman_condense()
           , c("lksdjfMFGGMBHCOINC", "MSlabCo", "IBMCorp", "MSlabCoGMBHCOKGlalal", "KABUSHIKIKAISHAMSlabCo"))
## --------<<  Condensing:2 ends here


