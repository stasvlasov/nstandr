## -------->>  [[file:../../nstandr.src.org::*Punctuation][Punctuation:2]]
expect_equal(c("WESTINGHOUSE, |.?^&*@ ELEC  "
             , "GRACE (W EN R) & CO - Ã²Ã¢ÃªÃ®Ã©"
             , "GRACE (W/R) & CO Ltd.") |> 
             cockburn_replace_punctuation()
           , c("WESTINGHOUSE &  ELEC  ", "GRACE W EN R  &  CO   oaeie", "GRACE W R  &  CO Ltd"))
## --------<<  Punctuation:2 ends here


