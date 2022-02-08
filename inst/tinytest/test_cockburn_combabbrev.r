## -------->>  [[file:../../harmonizer.src.org::*cockburn_combabbrev][cockburn_combabbrev:2]]
expect_equal(c(" A B Comp"
             , " A  B Comp a"
             , " I B M "
             , "I B M bla-bla n bla C O") |>
             cockburn_combabbrev()
           , c(`  A B Comp ` = "  AB Comp ", `  A  B Comp a ` = "  AB Comp a ", 
               `  I B M  ` = "  IBM  ", ` I B M bla-bla n bla C O ` = " IBM bla-bla n bla CO "
               ))



expect_equal(data.table(name = c("MÄKARÖNI ETÖ FKÜSNÖ Ltd"
                               , "MSLab CÖ. <a href=lsdldf> <br> <\\a>"
                               , " M S Lab Co."
                               , "MSLaeb Comp."
                               , "MSLab Comp. Ltd."
                               , "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝŸ") |> rep(20)
                      , foo = "I love coffee" ) |>
             cockburn_combabbrev()
           , structure(list(name = c(" MÄKARÖNI ETÖ FKÜSNÖ Ltd ", " MSLab CÖ. <a href=lsdldf> <br> <\\a> ", 
                                     "  MS Lab Co. ", " MSLaeb Comp. ", " MSLab Comp. Ltd. ", " ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝŸ ") |> rep(20)
                          , foo = c("I love coffee"|> rep(120))), row.names = c(NA, -120L), class = c("data.table", 
                                                                                                      "data.frame")))

expect_equal({
a <- c("MÄKARÖNI ETÖ FKÜSNÖ Ltd"
    , "MSLab CÖ. <a href=lsdldf> <br> <\\a>"
    , " M S Lab Co."
    , "MSLaeb Comp."
    , "MSLab Comp. Ltd."
    , "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝŸ") |>
    cockburn_combabbrev()
names(a) <- NULL
a
}
, c(" MÄKARÖNI ETÖ FKÜSNÖ Ltd ", " MSLab CÖ. <a href=lsdldf> <br> <\\a> ", 
"  MS Lab Co. ", " MSLaeb Comp. ", " MSLab Comp. Ltd. ", " ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝŸ "
)
)
## --------<<  cockburn_combabbrev:2 ends here


