## -------->>  [[file:../../harmonizer.src.org::*get_vector & inset_vector][get_vector & inset_vector:4]]
require("data.table")

inset_vector <- harmonizer:::inset_vector



## test placement
expect_equal(
    inset_vector(c("a", "b", "c", "d", "e")
               , data.table(c1 = c(1,2,3,4,5), x = c("x", "y", "z", "i", "j")))
  , data.table(c1 = c("a", "b", "c", "d", "e"), x = c("x", "y", "z", "i", "j")))

expect_equal(
    inset_vector(c("a", "b", "c", "d", "e")
               , data.table(c1 = c(1,2,3,4,5), x = c("x", "y", "z", "i", "j"))
               , placement = "prepend_to_col")
    , data.table(c1_harmonized = c("a", "b", "c", "d", "e"), c1 = c(1, 2, 3, 4, 5), x = c("x", "y", "z", "i", "j")))


expect_equal(
    inset_vector(c("a", "b", "c", "d", "e")
               , data.table(c1 = c(1,2,3,4,5), x = c("x", "y", "z", "i", "j"))
               , placement = "append_to_col")
  , data.table(c1 = c(1, 2, 3, 4, 5), c1_harmonized = c("a", 
                                                        "b", "c", "d", "e"), x = c("x", "y", "z", "i", "j")))


expect_equal(
    inset_vector(c("a", "b", "c", "d", "e")
               , data.table(c1 = c(1,2,3,4,5), x = c("x", "y", "z", "i", "j"))
               , placement = "append_to_x")
  , data.table(c1 = c(1, 2, 3, 4, 5), x = c("x", "y", "z", "i", 
                                            "j"), c1_harmonized = c("a", "b", "c", "d", "e")))

## test omited values
expect_error(inset_vector(c(1,1,1,1,1), data.table(c1 = c(5,5,5,5,5), x = c("a", "b", "c", "d", "e"))))

expect_equal(
    inset_vector(c("a", "b", "c", "d", "e")
               , data.table(c1 = c(1,2,3,4,5), x = c("x", "y", "z", "i", "j")))
  , data.table(c1 = c("a", "b", "c", "d", "e"), x = c("x", "y", "z", "i", "j")))

## case of all rows
expect_equal(
    inset_vector(c("a", "b", "c", "d", "e")
               , data.table(c1 = c(1,2,3,4,5), x = c("x", "y", "z", "i", "j"))
               , rows = c(TRUE, TRUE, TRUE, TRUE, TRUE))
  , data.table(c1 = c("a", "b", "c", "d", "e"), x = c("x", "y", "z", "i", "j")))

expect_equal(
    inset_vector(c("a", "b", "c", "d", "e")
               , data.table(c1 = c(1,2,3,4,5), x = c("x", "y", "z", "i", "j"))
               , rows = c(1, 2, 3, 4, 5))
  , data.table(c1 = c("a", "b", "c", "d", "e"), x = c("x", "y", "z", "i", "j")))


expect_error(
    inset_vector(c("a", "b", "c", "d", "e")
               , data.table(c1 = c(1,2,3,4,5), x = c("x", "y", "z", "i", "j"))
               , rows = c(5, 5, 3, 4, 5)))


expect_equal(
    inset_vector(c("a", "b", "c", "d", "e")
               , data.table(c1 = c(1,2,3,4,5), x = c("x", "y", "z", "i", "j"))
               , rows = c(3, 2, 1, 4, 5))
  , data.table(c1 = c("c", "b", "a", "d", "e"), x = c("x", "y", "z", "i", "j")))




## error for incorrect vector length
expect_error(
    inset_vector(c("a", "b", "c", "d", "e")
               , data.table(c1 = c(1,2,3,4,5), x = c("x", "y", "z", "i", "j"))
               , rows = c(TRUE, FALSE, TRUE, FALSE, TRUE)))

## subsetting rows
expect_equal(
    inset_vector(c("a", "b", "c")
               , data.table(c1 = c("1",2,3,4,5), x = c("x", "y", "z", "i", "j"))
               , rows = c(TRUE, FALSE, TRUE, FALSE, TRUE))
  , data.table(c1 = c("a", "2", "b", "4", "c"), x = c("x", "y", "z", "i", "j")))


expect_equal(
    inset_vector(c("a", "b", "c")
               , data.table(c1 = c("1",2,3,4,5), x = c("x", "y", "z", "i", "j"))
               , rows = c(1,3,5))
  , data.table(c1 = c("a", "2", "b", "4", "c"), x = c("x", "y", "z", "i", "j")))
## --------<<  get_vector & inset_vector:4 ends here


