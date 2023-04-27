## -------->>  [[file:../../nstandr.src.org::*standardize_dehtmlize][standardize_dehtmlize:2]]
expect_equal(c("abcd", "&amp; &apos; &gt;", "&amp;", "&euro; &lt;") |>
             standardize_dehtmlize()
, c("abcd", "& ' >", "&", "€ <"))
## --------<<  standardize_dehtmlize:2 ends here


