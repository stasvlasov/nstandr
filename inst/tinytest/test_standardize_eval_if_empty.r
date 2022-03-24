## -------->>  [[file:../../harmonizer.src.org::*standardize_empty][standardize_empty:2]]
standardize_is_data_empty <- nstandr:::standardize_is_data_empty
standardize_omit_empty <- nstandr:::standardize_omit_empty
standardize_eval_if_empty <- nstandr:::standardize_eval_if_empty

expect_equal(list("INCORPORATED", NULL, NULL, NULL, NULL) |> standardize_is_data_empty()
           , c(FALSE, TRUE, TRUE, TRUE, TRUE))

expect_equal(c(NA, "", 3,4, "wsd", NULL) |> standardize_is_data_empty()
           , c(TRUE, TRUE, FALSE, FALSE, FALSE))

expect_equal(list("INCORPORATED", NULL, NULL, NULL, NULL) |> standardize_omit_empty()
           , list("INCORPORATED"))

expect_equal((function() {
    a <- 5
    standardize_eval_if_empty("", a)
})()
, 5)

expect_equal((function() {
    a <- 5
    standardize_eval_if_empty("not_empty", a)
})()
, "not_empty")
## --------<<  standardize_empty:2 ends here


