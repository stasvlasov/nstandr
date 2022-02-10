## -------->>  [[file:../../harmonizer.src.org::*harmonize_empty][harmonize_empty:2]]
harmonize_is_data_empty <- harmonizer:::harmonize_is_data_empty
harmonize_omit_empty <- harmonizer:::harmonize_omit_empty
harmonize_eval_if_empty <- harmonizer:::harmonize_eval_if_empty

expect_equal(list("INCORPORATED", NULL, NULL, NULL, NULL) |> harmonize_is_data_empty()
           , c(FALSE, TRUE, TRUE, TRUE, TRUE))

expect_equal(c(NA, "", 3,4, "wsd", NULL) |> harmonize_is_data_empty()
           , c(TRUE, TRUE, FALSE, FALSE, FALSE))

expect_equal(list("INCORPORATED", NULL, NULL, NULL, NULL) |> harmonize_omit_empty()
           , list("INCORPORATED"))

expect_equal((function() {
    a <- 5
    harmonize_eval_if_empty("", a)
})()
, 5)

expect_equal((function() {
    a <- 5
    harmonize_eval_if_empty("not_empty", a)
})()
, "not_empty")
## --------<<  harmonize_empty:2 ends here


