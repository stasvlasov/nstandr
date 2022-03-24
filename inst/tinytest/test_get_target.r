## -------->>  [[file:../../harmonizer.src.org::*get_target & inset_target][get_target & inset_target:7]]
require("data.table")

get_target <- nstandr:::get_target

expect_equal(
    data.table(x.pro.30 = list(c(1,2,3,4), 2,3,4)
             , y = c(7,8,9,0)
             , x.pro.5 = c(0,0,0,0)) |>
    get_target(rows = c(T,T,F,T))
  , list(c(1, 2, 3, 4), 2, 4))

expect_equal(
    data.frame(c(1,2,3,4)
             , c("7","8","9","a")) |>
    get_target(col = 2
             , rows = c(T,T,F,T))
  , c("7", "8", "a"))


## test output_placement
expect_equal(
    data.frame(x = c(1,2,3,4)
             , std_x = c("7","8","9","a")) |>
    get_target(col = 1
             , rows = c(T,T,F,T)
             , output_placement = "append_to_col")
  , c("7", "8", "a"))

expect_equal(
    data.frame(x = c(1,2,3,4)
             , aaa = c("7","8","9","a")) |>
    get_target(col = 1
             , rows = c(T,T,F,T)
             , output_col_name = "aaa"
             , output_placement = "append_to_col")
  , c("7", "8", "a"))


expect_equal(
data.frame(x_standardized = c(1,2,3,4)
             , x = c("7","8","9","a")) |>
    get_target(col = 1
             , rows = c(T,T,F,T)
             , output_placement = "prepend_to_col")
, c(1,2,4))


## testing atomic
expect_equal(
 c("7","8","9","a") |>
    get_target(col = 1
             , rows = c(T,T,F,T)
             , output_placement = "prepend_to_col")
, c("7", "8", "a"))

expect_equal(
 c("7","8","9","a") |>
    get_target(col = 2
             , rows = c(T,T,F,T)
             , output_placement = "prepend_to_x")
  , c("7", "8", "a"))



expect_equal(
    data.frame(x = c(1,2,3,4)
             , y = TRUE
             , std_x = c("7","8","9","a")) |>
    get_target(col = 1
             , rows = c(T,T,F,T)
             , output_placement = "append_to_x")
  , c("7", "8", "a"))


expect_equal(
    data.frame(std_x = c(1,2,3,4)
             , y = TRUE
             , x = c("7","8","9","a")) |>
    get_target(col = 2
             , rows = c(T,T,F,T)
             , output_placement = "prepend_to_x")
  , c(1,2,4))
## --------<<  get_target & inset_target:7 ends here


