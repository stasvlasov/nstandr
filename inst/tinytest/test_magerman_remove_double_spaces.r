## -------->>  [[file:../../harmonizer.src.org::*magerman.remove.double.spaces][magerman.remove.double.spaces:2]]
expect_equal("  a   string with   many      douple    spaces      " |>
             magerman_remove_double_spaces()
           , " a string with many douple spaces ")
## --------<<  magerman.remove.double.spaces:2 ends here


