## -------->>  [[file:../../nstandr.src.org::*Compustat][Compustat:2]]
expect_equal(c("WESTINGHOUSE ELEC  "
           , "GRACE (W R) & CO"
           , "GRACE (W R) & CO Ltd.") |>
           cockburn_replace_compustat_names()
         , c(" WESTINGHOUSE ELECTRIC CORP. ", " W. R. GRACE & CO. ", "GRACE (W R) & CO Ltd."
             ))
## --------<<  Compustat:2 ends here


