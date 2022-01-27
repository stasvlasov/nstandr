## -------->>  [[file:../../harmonizer.src.org::*harmonize_x_split][harmonize_x_split:2]]
expect_equal(data.table(name = c("MÄKARÖNI ETÖ FKÜSNÖ Ltd"
                               , "MSLab CÖ. <a href=lsdldf> <br> <\\a>"
                               , "MSLab Co."
                               , "MSLaeb Comp."
                               , "MSLab Comp."
                               , "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝŸ") |>
                            rep(50)
                      , foo = "lalala" ) |>
             harmonize_x_split(10) |>
             sapply(class)
           , structure(c("data.table", "data.frame", "data.table", "data.frame", 
                         "data.table", "data.frame", "data.table", "data.frame", "data.table", 
                         "data.frame", "data.table", "data.frame", "data.table", "data.frame", 
                         "data.table", "data.frame", "data.table", "data.frame", "data.table", 
                         "data.frame", "data.table", "data.frame", "data.table", "data.frame", 
                         "data.table", "data.frame", "data.table", "data.frame", "data.table", 
                         "data.frame", "data.table", "data.frame", "data.table", "data.frame", 
                         "data.table", "data.frame", "data.table", "data.frame", "data.table", 
                         "data.frame", "data.table", "data.frame", "data.table", "data.frame", 
                         "data.table", "data.frame", "data.table", "data.frame", "data.table", 
                         "data.frame", "data.table", "data.frame", "data.table", "data.frame", 
                         "data.table", "data.frame", "data.table", "data.frame", "data.table", 
                         "data.frame"), .Dim = c(2L, 30L), .Dimnames = list(NULL, c("1", 
                                                                                    "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", 
                                                                                    "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", 
                                                                                    "25", "26", "27", "28", "29", "30"))))



expect_equal(c("MÄKARÖNI ETÖ FKÜSNÖ Ltd"
             , "MSLab CÖ. <a href=lsdldf> <br> <\\a>"
             , "MSLab Co."
             , "MSLaeb Comp."
             , "MSLab Comp."
             , "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝŸ") |>
             rep(50) |>
             harmonize_x_split(10)
, list(`1` = c("MÄKARÖNI ETÖ FKÜSNÖ Ltd", "MSLab CÖ. <a href=lsdldf> <br> <\\a>", 
"MSLab Co.", "MSLaeb Comp.", "MSLab Comp.", "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝŸ", 
"MÄKARÖNI ETÖ FKÜSNÖ Ltd", "MSLab CÖ. <a href=lsdldf> <br> <\\a>", 
"MSLab Co.", "MSLaeb Comp."), `2` = c("MSLab Comp.", "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝŸ", 
"MÄKARÖNI ETÖ FKÜSNÖ Ltd", "MSLab CÖ. <a href=lsdldf> <br> <\\a>", 
"MSLab Co.", "MSLaeb Comp.", "MSLab Comp.", "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝŸ", 
"MÄKARÖNI ETÖ FKÜSNÖ Ltd", "MSLab CÖ. <a href=lsdldf> <br> <\\a>"
), `3` = c("MSLab Co.", "MSLaeb Comp.", "MSLab Comp.", "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝŸ", 
"MÄKARÖNI ETÖ FKÜSNÖ Ltd", "MSLab CÖ. <a href=lsdldf> <br> <\\a>", 
"MSLab Co.", "MSLaeb Comp.", "MSLab Comp.", "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝŸ"
), `4` = c("MÄKARÖNI ETÖ FKÜSNÖ Ltd", "MSLab CÖ. <a href=lsdldf> <br> <\\a>", 
"MSLab Co.", "MSLaeb Comp.", "MSLab Comp.", "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝŸ", 
"MÄKARÖNI ETÖ FKÜSNÖ Ltd", "MSLab CÖ. <a href=lsdldf> <br> <\\a>", 
"MSLab Co.", "MSLaeb Comp."), `5` = c("MSLab Comp.", "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝŸ", 
"MÄKARÖNI ETÖ FKÜSNÖ Ltd", "MSLab CÖ. <a href=lsdldf> <br> <\\a>", 
"MSLab Co.", "MSLaeb Comp.", "MSLab Comp.", "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝŸ", 
"MÄKARÖNI ETÖ FKÜSNÖ Ltd", "MSLab CÖ. <a href=lsdldf> <br> <\\a>"
), `6` = c("MSLab Co.", "MSLaeb Comp.", "MSLab Comp.", "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝŸ", 
"MÄKARÖNI ETÖ FKÜSNÖ Ltd", "MSLab CÖ. <a href=lsdldf> <br> <\\a>", 
"MSLab Co.", "MSLaeb Comp.", "MSLab Comp.", "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝŸ"
), `7` = c("MÄKARÖNI ETÖ FKÜSNÖ Ltd", "MSLab CÖ. <a href=lsdldf> <br> <\\a>", 
"MSLab Co.", "MSLaeb Comp.", "MSLab Comp.", "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝŸ", 
"MÄKARÖNI ETÖ FKÜSNÖ Ltd", "MSLab CÖ. <a href=lsdldf> <br> <\\a>", 
"MSLab Co.", "MSLaeb Comp."), `8` = c("MSLab Comp.", "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝŸ", 
"MÄKARÖNI ETÖ FKÜSNÖ Ltd", "MSLab CÖ. <a href=lsdldf> <br> <\\a>", 
"MSLab Co.", "MSLaeb Comp.", "MSLab Comp.", "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝŸ", 
"MÄKARÖNI ETÖ FKÜSNÖ Ltd", "MSLab CÖ. <a href=lsdldf> <br> <\\a>"
), `9` = c("MSLab Co.", "MSLaeb Comp.", "MSLab Comp.", "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝŸ", 
"MÄKARÖNI ETÖ FKÜSNÖ Ltd", "MSLab CÖ. <a href=lsdldf> <br> <\\a>", 
"MSLab Co.", "MSLaeb Comp.", "MSLab Comp.", "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝŸ"
), `10` = c("MÄKARÖNI ETÖ FKÜSNÖ Ltd", "MSLab CÖ. <a href=lsdldf> <br> <\\a>", 
"MSLab Co.", "MSLaeb Comp.", "MSLab Comp.", "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝŸ", 
"MÄKARÖNI ETÖ FKÜSNÖ Ltd", "MSLab CÖ. <a href=lsdldf> <br> <\\a>", 
"MSLab Co.", "MSLaeb Comp."), `11` = c("MSLab Comp.", "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝŸ", 
"MÄKARÖNI ETÖ FKÜSNÖ Ltd", "MSLab CÖ. <a href=lsdldf> <br> <\\a>", 
"MSLab Co.", "MSLaeb Comp.", "MSLab Comp.", "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝŸ", 
"MÄKARÖNI ETÖ FKÜSNÖ Ltd", "MSLab CÖ. <a href=lsdldf> <br> <\\a>"
), `12` = c("MSLab Co.", "MSLaeb Comp.", "MSLab Comp.", "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝŸ", 
"MÄKARÖNI ETÖ FKÜSNÖ Ltd", "MSLab CÖ. <a href=lsdldf> <br> <\\a>", 
"MSLab Co.", "MSLaeb Comp.", "MSLab Comp.", "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝŸ"
), `13` = c("MÄKARÖNI ETÖ FKÜSNÖ Ltd", "MSLab CÖ. <a href=lsdldf> <br> <\\a>", 
"MSLab Co.", "MSLaeb Comp.", "MSLab Comp.", "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝŸ", 
"MÄKARÖNI ETÖ FKÜSNÖ Ltd", "MSLab CÖ. <a href=lsdldf> <br> <\\a>", 
"MSLab Co.", "MSLaeb Comp."), `14` = c("MSLab Comp.", "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝŸ", 
"MÄKARÖNI ETÖ FKÜSNÖ Ltd", "MSLab CÖ. <a href=lsdldf> <br> <\\a>", 
"MSLab Co.", "MSLaeb Comp.", "MSLab Comp.", "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝŸ", 
"MÄKARÖNI ETÖ FKÜSNÖ Ltd", "MSLab CÖ. <a href=lsdldf> <br> <\\a>"
), `15` = c("MSLab Co.", "MSLaeb Comp.", "MSLab Comp.", "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝŸ", 
"MÄKARÖNI ETÖ FKÜSNÖ Ltd", "MSLab CÖ. <a href=lsdldf> <br> <\\a>", 
"MSLab Co.", "MSLaeb Comp.", "MSLab Comp.", "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝŸ"
), `16` = c("MÄKARÖNI ETÖ FKÜSNÖ Ltd", "MSLab CÖ. <a href=lsdldf> <br> <\\a>", 
"MSLab Co.", "MSLaeb Comp.", "MSLab Comp.", "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝŸ", 
"MÄKARÖNI ETÖ FKÜSNÖ Ltd", "MSLab CÖ. <a href=lsdldf> <br> <\\a>", 
"MSLab Co.", "MSLaeb Comp."), `17` = c("MSLab Comp.", "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝŸ", 
"MÄKARÖNI ETÖ FKÜSNÖ Ltd", "MSLab CÖ. <a href=lsdldf> <br> <\\a>", 
"MSLab Co.", "MSLaeb Comp.", "MSLab Comp.", "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝŸ", 
"MÄKARÖNI ETÖ FKÜSNÖ Ltd", "MSLab CÖ. <a href=lsdldf> <br> <\\a>"
), `18` = c("MSLab Co.", "MSLaeb Comp.", "MSLab Comp.", "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝŸ", 
"MÄKARÖNI ETÖ FKÜSNÖ Ltd", "MSLab CÖ. <a href=lsdldf> <br> <\\a>", 
"MSLab Co.", "MSLaeb Comp.", "MSLab Comp.", "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝŸ"
), `19` = c("MÄKARÖNI ETÖ FKÜSNÖ Ltd", "MSLab CÖ. <a href=lsdldf> <br> <\\a>", 
"MSLab Co.", "MSLaeb Comp.", "MSLab Comp.", "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝŸ", 
"MÄKARÖNI ETÖ FKÜSNÖ Ltd", "MSLab CÖ. <a href=lsdldf> <br> <\\a>", 
"MSLab Co.", "MSLaeb Comp."), `20` = c("MSLab Comp.", "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝŸ", 
"MÄKARÖNI ETÖ FKÜSNÖ Ltd", "MSLab CÖ. <a href=lsdldf> <br> <\\a>", 
"MSLab Co.", "MSLaeb Comp.", "MSLab Comp.", "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝŸ", 
"MÄKARÖNI ETÖ FKÜSNÖ Ltd", "MSLab CÖ. <a href=lsdldf> <br> <\\a>"
), `21` = c("MSLab Co.", "MSLaeb Comp.", "MSLab Comp.", "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝŸ", 
"MÄKARÖNI ETÖ FKÜSNÖ Ltd", "MSLab CÖ. <a href=lsdldf> <br> <\\a>", 
"MSLab Co.", "MSLaeb Comp.", "MSLab Comp.", "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝŸ"
), `22` = c("MÄKARÖNI ETÖ FKÜSNÖ Ltd", "MSLab CÖ. <a href=lsdldf> <br> <\\a>", 
"MSLab Co.", "MSLaeb Comp.", "MSLab Comp.", "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝŸ", 
"MÄKARÖNI ETÖ FKÜSNÖ Ltd", "MSLab CÖ. <a href=lsdldf> <br> <\\a>", 
"MSLab Co.", "MSLaeb Comp."), `23` = c("MSLab Comp.", "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝŸ", 
"MÄKARÖNI ETÖ FKÜSNÖ Ltd", "MSLab CÖ. <a href=lsdldf> <br> <\\a>", 
"MSLab Co.", "MSLaeb Comp.", "MSLab Comp.", "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝŸ", 
"MÄKARÖNI ETÖ FKÜSNÖ Ltd", "MSLab CÖ. <a href=lsdldf> <br> <\\a>"
), `24` = c("MSLab Co.", "MSLaeb Comp.", "MSLab Comp.", "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝŸ", 
"MÄKARÖNI ETÖ FKÜSNÖ Ltd", "MSLab CÖ. <a href=lsdldf> <br> <\\a>", 
"MSLab Co.", "MSLaeb Comp.", "MSLab Comp.", "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝŸ"
), `25` = c("MÄKARÖNI ETÖ FKÜSNÖ Ltd", "MSLab CÖ. <a href=lsdldf> <br> <\\a>", 
"MSLab Co.", "MSLaeb Comp.", "MSLab Comp.", "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝŸ", 
"MÄKARÖNI ETÖ FKÜSNÖ Ltd", "MSLab CÖ. <a href=lsdldf> <br> <\\a>", 
"MSLab Co.", "MSLaeb Comp."), `26` = c("MSLab Comp.", "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝŸ", 
"MÄKARÖNI ETÖ FKÜSNÖ Ltd", "MSLab CÖ. <a href=lsdldf> <br> <\\a>", 
"MSLab Co.", "MSLaeb Comp.", "MSLab Comp.", "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝŸ", 
"MÄKARÖNI ETÖ FKÜSNÖ Ltd", "MSLab CÖ. <a href=lsdldf> <br> <\\a>"
), `27` = c("MSLab Co.", "MSLaeb Comp.", "MSLab Comp.", "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝŸ", 
"MÄKARÖNI ETÖ FKÜSNÖ Ltd", "MSLab CÖ. <a href=lsdldf> <br> <\\a>", 
"MSLab Co.", "MSLaeb Comp.", "MSLab Comp.", "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝŸ"
), `28` = c("MÄKARÖNI ETÖ FKÜSNÖ Ltd", "MSLab CÖ. <a href=lsdldf> <br> <\\a>", 
"MSLab Co.", "MSLaeb Comp.", "MSLab Comp.", "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝŸ", 
"MÄKARÖNI ETÖ FKÜSNÖ Ltd", "MSLab CÖ. <a href=lsdldf> <br> <\\a>", 
"MSLab Co.", "MSLaeb Comp."), `29` = c("MSLab Comp.", "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝŸ", 
"MÄKARÖNI ETÖ FKÜSNÖ Ltd", "MSLab CÖ. <a href=lsdldf> <br> <\\a>", 
"MSLab Co.", "MSLaeb Comp.", "MSLab Comp.", "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝŸ", 
"MÄKARÖNI ETÖ FKÜSNÖ Ltd", "MSLab CÖ. <a href=lsdldf> <br> <\\a>"
), `30` = c("MSLab Co.", "MSLaeb Comp.", "MSLab Comp.", "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝŸ", 
"MÄKARÖNI ETÖ FKÜSNÖ Ltd", "MSLab CÖ. <a href=lsdldf> <br> <\\a>", 
"MSLab Co.", "MSLaeb Comp.", "MSLab Comp.", "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝŸ"
)))
## --------<<  harmonize_x_split:2 ends here


