## -------->>  [[file:../../harmonizer.src.org::*get_vector & inset_vector][get_vector & inset_vector:3]]
require("data.table")

get_vector <- harmonizer:::get_vector

expect_equal(
    data.table(x.pro.30 = list(c(1,2,3,4), 2,3,4)
             , y = c(7,8,9,0)
             , x.pro.5 = c(0,0,0,0)) |>
    get_vector(rows = c(T,T,F,T))
  , list(c(1, 2, 3, 4), 2, 4))

expect_equal(
    data.frame(c(1,2,3,4)
             , c("7","8","9","a")) |>
    get_vector(col = 2
             , rows = c(T,T,F,T))
  , c("7", "8", "a"))


## test placement

expect_equal(
    data.frame(x = c(1,2,3,4)
             , x_harmonized = c("7","8","9","a")) |>
    get_vector(col = 1
             , rows = c(T,T,F,T)
             , placement = "append_to_col")
  , c("7", "8", "a"))

expect_equal(
    data.frame(x = c(1,2,3,4)
             , aaa = c("7","8","9","a")) |>
    get_vector(col = 1
             , rows = c(T,T,F,T)
               , name = "aaa"
             , placement = "append_to_col")
  , c("7", "8", "a"))


expect_equal(
data.frame(x_harmonized = c(1,2,3,4)
             , x = c("7","8","9","a")) |>
    get_vector(col = 1
             , rows = c(T,T,F,T)
             , placement = "prepend_to_col")
  , c(1,2,4))



expect_equal(
    data.frame(x = c(1,2,3,4)
             , y = TRUE
             , x_harmonized = c("7","8","9","a")) |>
    get_vector(col = 1
             , rows = c(T,T,F,T)
             , placement = "append_to_x")
  , c("7", "8", "a"))


expect_equal(
    data.frame(x_harmonized = c(1,2,3,4)
             , y = TRUE
             , x = c("7","8","9","a")) |>
    get_vector(col = 2
             , rows = c(T,T,F,T)
             , placement = "prepend_to_x")
  , c(1,2,4))
## --------<<  get_vector & inset_vector:3 ends here


