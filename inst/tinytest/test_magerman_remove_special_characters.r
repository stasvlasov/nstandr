## -------->>  [[file:../../nstandr.src.org::*magerman.remove.special.characters][magerman.remove.special.characters:2]]
expect_equal("\t©𝍎၍\t%^_\n\t" |>
             magerman_remove_special_characters()
           , "")
## --------<<  magerman.remove.special.characters:2 ends here


