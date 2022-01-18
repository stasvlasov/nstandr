## -------->>  [[file:../../harmonizer.src.org::*magerman.replace.comma.period.irregularities.*][magerman.replace.comma.period.irregularities.*:2]]
expect_equal("A sentence with .irregular punctuation ,like commas , and periods ." |>
             magerman_replace_comma_period_irregularities_all()
           , "A sentence with irregular punctuation, like commas, and periods ")
## --------<<  magerman.replace.comma.period.irregularities.*:2 ends here


