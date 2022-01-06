## -------->>  [[file:../../harmonizer.src.org::*get_vector][get_vector:2]]
## missing col
expect_error(get_vector(c("a", "b", "c")))

## select rows
expect_equal(
    get_vector(c("a", "b", "c"), 1, rows = c(1,3))
  , c("a", "c"))

## select col
expect_equal(
    data.table(NA
             , c("a", "b", "c")
             , c(1,2,3)) |>
    get_vector(2, rows = c(1,3))
  , c("a", "c"))

## fallback
expect_equal(
    data.table(NA
             , c("a", "b", "c")
             , c(1,2,3)) |>
    get_vector(2, rows = c(1,3), fallback_value = c("x"))
  , c("a", "c"))


expect_equal(
    data.table(NA
             , c("a", "b", "c")
             , c(1,2,3)) |>
    get_vector(2, rows = c(1,3)
             , fallback_value = c("x")
             , fallback_value_ignored_if_col = FALSE)
  , c("x", "x"))

expect_equal(
    data.table(NA
             , c("a", "b", "c")
             , c(1,2,3)) |>
    get_vector(NULL, rows = c(1,3)
             , fallback_value = c("x"))
  , c("x", "x"))

expect_equal(
    data.table(NA
             , c("a", "b", "c")
             , c(1,2,3)) |>
    get_vector(NULL, rows = c(1,3)
             , fallback_value = c("x", "y", "z"))
  , c("x", "z"))

## choises
expect_error(
    data.table(NA
             , c("a", "b", "c")
             , c(1,2,3)) |>
    get_vector(NULL, rows = c(1,3)
             , fallback_value = c("x")
             , choices = "a"
             , fallback_value_ignored_if_col = FALSE))

expect_error(
    data.table(NA
             , c("a", "b", "c")
             , c(1,2,3)) |>
    get_vector(2, rows = c(1,3)
             , choices = "x"))
## --------<<  get_vector:2 ends here


