## -------->>  [[file:../../harmonizer.src.org::*unlist_if_possible][unlist_if_possible:2]]
expect_equal(c(1,2,3,4) |> unlist_if_possible()
           , c(1, 2, 3, 4))

expect_equal(list(c("a"), NULL, 3, "5", character(0)) |> unlist_if_possible()
           , c("a", NA, "3", "5", NA))


expect_equal(list(c("a"), 3, "5") |> unlist_if_possible()
           , c("a", "3", "5"))


expect_equal(list(c("a", "b", "c"), NULL, 3, "5", character(0)) |> unlist_if_possible()
           , list(c("a", "b", "c"), NULL, 3, "5", character(0)))


expect_equal(list(c("a", NA, ""), NULL, 3, "5", character(0)) |> unlist_if_possible()
           , c("a", NA, "3", "5", NA))


expect_equal(list(c("a", NA, ""), NULL, 3, "5", character(0)) |> unlist_if_possible(remove_empty_values = FALSE)
           , list(c("a", NA, ""), NULL, 3, "5", character(0)))
## --------<<  unlist_if_possible:2 ends here


