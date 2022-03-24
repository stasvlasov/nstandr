## -------->>  [[file:../../harmonizer.src.org::*get_target & inset_target][get_target & inset_target:3]]
replace_fixed_if_string <- nstandr:::replace_fixed_if_string

a <- NA
expect_equal(replace_fixed_if_string(a, "df_a_sd"), "")
expect_equal(replace_fixed_if_string(a), "")

a <- "Hi!"
expect_equal(replace_fixed_if_string(a, "df_a_sd"), "df_Hi!_sd")
expect_equal(replace_fixed_if_string(a), "Hi!")
## --------<<  get_target & inset_target:3 ends here


