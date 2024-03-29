## -------->>  [[file:../../nstandr.src.org::*standardize_toupper][standardize_toupper:2]]
expect_equal(data.table(name = c("MÄKARÖNI ETÖ FKÜSNÖ Ltd"
                               , "MSLab CÖ. <a href=lsdldf> <br> <\\a>"
                               , "MSLab Co."
                               , "MSLaeb Comp."
                               , "MSLab Comp."
                               , "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝŸ")
                      , foo = "lalala" ) |>
             standardize_toupper(col = 2, name = "bar")
           , structure(list(name = c("MÄKARÖNI ETÖ FKÜSNÖ Ltd", "MSLab CÖ. <a href=lsdldf> <br> <\\a>", 
                                     "MSLab Co.", "MSLaeb Comp.", "MSLab Comp.", "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝŸ"
                                     ), foo = c("LALALA", "LALALA", "LALALA", "LALALA", "LALALA", 
                                                "LALALA")), row.names = c(NA, -6L), class = c("data.table", "data.frame"
                                                                                              )))
## --------<<  standardize_toupper:2 ends here


