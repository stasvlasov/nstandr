## -------->>  [[file:../../harmonizer.src.org::*magerman.remove.special.characters][magerman.remove.special.characters:2]]
expect_equal("LK \tD©𝍎 ၍\tF:'\";092834!@#$%^&*()_+-\n\t" |>
             magerman_remove_special_characters()
           , "LK D F:'\";092834!@#&*()+-")
## --------<<  magerman.remove.special.characters:2 ends here


