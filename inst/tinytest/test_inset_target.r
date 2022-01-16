## -------->>  [[file:../../harmonizer.src.org::*get_target & inset_target][get_target & inset_target:4]]
require("data.table")

inset_target <- harmonizer:::inset_target


## test vectors
expect_equal(
    inset_target(c("a", "b", "c", "d", "e")
               , c("1",2,3,4,5))
  , c("a", "b", "c", "d", "e"))


expect_equal(
    inset_target(c("a", "b",  "d", "e")
               , c("1",2,3,4,5)
               , rows = c(1,2,4,5))
  , c("a", "b", "3", "d", "e"))


## test placement
expect_equal(
    inset_target(c("a", "b", "c", "d", "e")
               , data.table(c1 = c(1,2,3,4,5), x = c("x", "y", "z", "i", "j")))
  , data.table(c1 = c("a", "b", "c", "d", "e"), x = c("x", "y", "z", "i", "j")))

expect_equal(
    inset_target(c("a", "b", "c", "d", "e")
               , data.table(c1 = c(1,2,3,4,5), x = c("x", "y", "z", "i", "j"))
               , placement = "prepend_to_col")
    , data.table(c1_harmonized = c("a", "b", "c", "d", "e"), c1 = c(1, 2, 3, 4, 5), x = c("x", "y", "z", "i", "j")))


expect_equal(
    inset_target(c("a", "b", "c", "d", "e")
               , data.table(c1 = c(1,2,3,4,5), x = c("x", "y", "z", "i", "j"))
               , placement = "append_to_col")
  , data.table(c1 = c(1, 2, 3, 4, 5), c1_harmonized = c("a", 
                                                        "b", "c", "d", "e"), x = c("x", "y", "z", "i", "j")))


expect_equal(
    inset_target(c("a", "b", "c", "d", "e")
               , data.table(c1 = c(1,2,3,4,5), x = c("x", "y", "z", "i", "j"))
               , placement = "append_to_x")
  , data.table(c1 = c(1, 2, 3, 4, 5), x = c("x", "y", "z", "i", 
                                            "j"), c1_harmonized = c("a", "b", "c", "d", "e")))

## test omited values

expect_equal(
    inset_target(c("a", "b", "c", "d", "e")
               , data.table(c1 = c(1,2,3,4,5), x = c("x", "y", "z", "i", "j")))
  , data.table(c1 = c("a", "b", "c", "d", "e"), x = c("x", "y", "z", "i", "j")))

## case of all rows
expect_equal(
    inset_target(c("a", "b", "c", "d", "e")
               , data.table(c1 = c(1,2,3,4,5), x = c("x", "y", "z", "i", "j"))
               , rows = c(TRUE, TRUE, TRUE, TRUE, TRUE))
  , data.table(c1 = c("a", "b", "c", "d", "e"), x = c("x", "y", "z", "i", "j")))

expect_equal(
    inset_target(c("a", "b", "c", "d", "e")
               , data.table(c1 = c(1,2,3,4,5), x = c("x", "y", "z", "i", "j"))
               , rows = c(1, 2, 3, 4, 5))
  , data.table(c1 = c("a", "b", "c", "d", "e"), x = c("x", "y", "z", "i", "j")))


expect_error(
    inset_target(c("a", "b", "c", "d", "e")
               , data.table(c1 = c(1,2,3,4,5), x = c("x", "y", "z", "i", "j"))
               , rows = c(5, 5, 3, 4, 5)))


expect_equal(
    inset_target(c("a", "b", "c", "d", "e")
               , data.table(c1 = c(1,2,3,4,5), x = c("x", "y", "z", "i", "j"))
               , rows = c(3, 2, 1, 4, 5))
  , data.table(c1 = c("c", "b", "a", "d", "e"), x = c("x", "y", "z", "i", "j")))




## error for incorrect vector length
expect_error(inset_target(c("a", "b", "c", "d", "e")
               , data.table(c1 = c(1,2,3,4,5), x = c("x", "y", "z", "i", "j"))
               , rows = c(TRUE, FALSE, TRUE, FALSE, TRUE)))

## subsetting rows
expect_equal(
    inset_target(c("a", "b", "c")
               , data.table(c1 = c("1",2,3,4,5), x = c("x", "y", "z", "i", "j"))
               , rows = c(TRUE, FALSE, TRUE, FALSE, TRUE))
  , data.table(c1 = c("a", "2", "b", "4", "c"), x = c("x", "y", "z", "i", "j")))


expect_equal(
    inset_target(c("a", "b", "c")
               , data.table(c1 = c("1",2,3,4,5), x = c("x", "y", "z", "i", "j"))
               , rows = c(1,3,5))
  , data.table(c1 = c("a", "2", "b", "4", "c"), x = c("x", "y", "z", "i", "j")))
## --------<<  get_target & inset_target:4 ends here


