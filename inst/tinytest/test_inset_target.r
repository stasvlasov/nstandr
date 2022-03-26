## -------->>  [[file:../../nstandr.src.org::*get_target & inset_target][get_target & inset_target:8]]
require("data.table")

inset_target <- nstandr:::inset_target


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



    expect_equal(inset_target(c("a", "b",  "d", "e")
                            , c("1",2,3,4,5)
                            , output_placement = "prepend_to_col"
                            , rows = c(1,2,4,5))
               , structure(list(std_x = c("a", "b", "3", "d", "e"), x = c("1", 
                                                                           "2", "3", "4", "5")), row.names = c(NA, -5L), class = c("data.table", 
                                                                                                                                   "data.frame")))





## test output_placement
expect_equal(
    inset_target(c("a", "b", "c", "d", "e")
               , data.table(c1 = c(1,2,3,4,5), x = c("x", "y", "z", "i", "j")))
  , data.table(c1 = c("a", "b", "c", "d", "e"), x = c("x", "y", "z", "i", "j")))

expect_equal(
    inset_target(c("a", "b", "c", "d", "e")
               , data.table(c1 = c(1,2,3,4,5), x = c("x", "y", "z", "i", "j"))
               , output_placement = "prepend_to_col")
    , data.table(std_c1 = c("a", "b", "c", "d", "e"), c1 = c(1, 2, 3, 4, 5), x = c("x", "y", "z", "i", "j")))


expect_equal(
    inset_target(c("a", "b", "c", "d", "e")
               , data.table(c1 = c(1,2,3,4,5), x = c("x", "y", "z", "i", "j"))
               , output_placement = "append_to_col")
  , data.table(c1 = c(1, 2, 3, 4, 5), std_c1 = c("a", 
                                                        "b", "c", "d", "e"), x = c("x", "y", "z", "i", "j")))


expect_equal(
    inset_target(c("a", "b", "c", "d", "e")
               , data.table(c1 = c(1,2,3,4,5), x = c("x", "y", "z", "i", "j"))
               , output_placement = "append_to_x")
  , data.table(c1 = c(1, 2, 3, 4, 5), x = c("x", "y", "z", "i", 
                                            "j"), std_c1 = c("a", "b", "c", "d", "e")))

## test omited values

expect_equal(
    inset_target(c("a", "b", "c")
               , data.table(c1 = c(1,2,3,4,5), x = c("x", "y", "z", "i", "j"))
               , rows = c(1, 2, 5)
               , omitted_rows_value = NA)
, structure(list(c1 = c("a", "b", NA, NA, "c"), x = c("x", "y", 
"z", "i", "j")), row.names = c(NA, -5L), class = c("data.table", 
"data.frame"))
)

expect_equal(
    inset_target(c("a", "b", "c")
               , data.table(c1 = c(1,2,3,4,5), x = c("x", "y", "z", "i", "j"))
               , rows = c(1, 2, 5)
               , output_placement = "append_to_x"
               , output_col_name = "codes.new"
               , omitted_rows_value = NULL
               )
, structure(list(c1 = c(1, 2, 3, 4, 5), x = c("x", "y", "z", "i", 
"j"), codes.new = c("a", "b", "3", "4", "c")), row.names = c(NA, 
-5L), class = c("data.table", "data.frame"))
)

expect_equal(
    inset_target(c("a", "b", "c")
               , data.table(c1 = c(1,2,3,4,5), x = c("x", "y", "z", "i", "j"))
               , rows = c(1, 2, 5)
               , output_placement = "append_to_x"
               , output_col_name = "codes.new"
               , omitted_rows_value = NULL
               , omitted_rows_value_for_new_col = NA_character_
               )
, structure(list(c1 = c(1, 2, 3, 4, 5), x = c("x", "y", "z", "i", 
"j"), codes.new = c("a", "b", NA, NA, "c")), row.names = c(NA, 
-5L), class = c("data.table", "data.frame"))
)



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


## test returning only target column
expect_equal(inset_target(c("a", "b", "c")
                        , data.table(c1 = c("1",2,3,4,5), x = c("x", "y", "z", "i", "j"))
                        , rows = c(1,3,5)
                        , return_only_target_col = TRUE)
           , c("a", "2", "b", "4", "c"))

## test atomic names
expect_equal(
    inset_target(c("a", "b", "c")
               , c("1",2,3,4,5)
               , rows = c(1,3,5))
           , c("a", "2", "b", "4", "c"))


expect_equal(inset_target(c("a", "b", "c")
                            , c("1",2,3,4,5)
                            , rows = c(1,3,5)
                            , output_placement = "prepend_to_col")
           , structure(list(std_x = c("a", "2", "b", "4", "c"), x = c("1", 
                                                                       "2", "3", "4", "5")), row.names = c(NA, -5L), class = c("data.table", 
                                                                                                                               "data.frame")))

## test omited
## --------<<  get_target & inset_target:8 ends here


