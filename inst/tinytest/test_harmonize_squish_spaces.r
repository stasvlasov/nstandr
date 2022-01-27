## -------->>  [[file:../../harmonizer.src.org::*harmonize_squish_spaces][harmonize_squish_spaces:2]]
expect_equal(harmonize_squish_spaces("  String with trailing,  middle, and leading white space\t"
                                   , wrap_in_spaces = TRUE)
           , " String with trailing, middle, and leading white space ")




expect_equal(harmonize_squish_spaces("\n\nString with excess,  trailing and leading white   space\n\n"
                                   , wrap_in_spaces = FALSE)
           , "String with excess, trailing and leading white space")
## --------<<  harmonize_squish_spaces:2 ends here


