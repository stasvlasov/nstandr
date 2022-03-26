## -------->>  [[file:../../nstandr.src.org::*magerman.replace.comma.period.irregularities.*][magerman.replace.comma.period.irregularities.*:4]]
expect_equal(c("MSlab ,INC. ,LTD"
             , "MSlab ,LTD Universe") |>
             magerman_replace_comma_period_irregularities()
             , c("MSlab, INC. , LTD", "MSlab ,LTD Universe"))
## --------<<  magerman.replace.comma.period.irregularities.*:4 ends here


