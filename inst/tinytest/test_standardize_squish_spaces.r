## -------->>  [[file:../../nstandr.src.org::*standardize_squish_spaces][standardize_squish_spaces:2]]
expect_equal(standardize_squish_spaces("  String with trailing,  middle, and leading white space\t"
                                   , wrap_in_spaces = TRUE)
           , " String with trailing, middle, and leading white space ")




expect_equal(standardize_squish_spaces("\n\nString with excess,  trailing and leading white   space\n\n"
                                   , wrap_in_spaces = FALSE)
           , "String with excess, trailing and leading white space")
## --------<<  standardize_squish_spaces:2 ends here


