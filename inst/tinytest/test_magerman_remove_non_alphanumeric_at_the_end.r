## -------->>  [[file:../../nstandr.src.org::*magerman.remove.non.alphanumeric.*][magerman.remove.non.alphanumeric.*:2]]
expect_equal(c("MSLab Co. :"
             , "MSLab Co.++"
             , "MSLab Co.*&^") |>
             magerman_remove_non_alphanumeric_at_the_end()
           , c("MSLab Co.", "MSLab Co.", "MSLab Co."))

expect_equal(c("_MSLab Co."
             , "?MSLab Co."
             , ".-:MSLab Co.") |>
             magerman_remove_non_alphanumeric_at_the_beginning()
           , c("MSLab Co.", "MSLab Co.", "MSLab Co."))
## --------<<  magerman.remove.non.alphanumeric.*:2 ends here


