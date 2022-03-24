## -------->>  [[file:../../harmonizer.src.org::*get_target & inset_target][get_target & inset_target:4]]
make_indexed_col_name <- nstandr:::make_indexed_col_name

expect_equal(make_indexed_col_name("a"), "a")

expect_equal(make_indexed_col_name("a", "b"), "a")

expect_equal(make_indexed_col_name("a", "a"), "a_1")

expect_equal(make_indexed_col_name("a", "a", index_init_val = 0L), "a_0")

expect_equal(make_indexed_col_name("a", "a_11"), "a_12")

expect_equal(make_indexed_col_name("a", c(NA, "a", "a_41","a_11")), "a_42")
## --------<<  get_target & inset_target:4 ends here


