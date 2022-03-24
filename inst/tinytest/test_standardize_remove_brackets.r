## -------->>  [[file:../../harmonizer.src.org::*standardize_remove_brackets][standardize_remove_brackets:2]]
## remove.brackets breaks the encoding (so it is better to apply decoding first)
expect_equal(standardize_remove_brackets("fa\xE7ile (lalala) lkj (sdfs) AAA [sdf]")
           , "fa�ile  lkj  AAA ")

expect_equal(standardize_remove_brackets("fa7ile (lalala) lkj (sdfs) AAA [sdf]")
           , "fa7ile  lkj  AAA ")
## --------<<  standardize_remove_brackets:2 ends here


