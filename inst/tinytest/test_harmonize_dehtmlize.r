## -------->>  [[file:../../harmonizer.src.org::*harmonize_dehtmlize][harmonize_dehtmlize:2]]
expect_equal(c("abcd", "&amp; &apos; &gt;", "&amp;", "&euro; &lt;") |>
             harmonize_dehtmlize()
, c("abcd", "& ' >", "&", "€ <"))
## --------<<  harmonize_dehtmlize:2 ends here


