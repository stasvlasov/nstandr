## -------->>  [[file:../../nstandr.src.org::*standardize_make_procedures_list][standardize_make_procedures_list:2]]
standardize_make_procedures_list <- nstandr:::standardize_make_procedures_list

expect_equal(data.frame(no = c(3,2,"-", "")
                      , message = c("hello", "world", "man", "dfsdf")
                      , function.call = c("'c', 1, b=3", "'sum', 8,8,9", "'version'", "")) |>
             standardize_make_procedures_list()
           , list(world = list("sum", 8, 8, 9), hello = list("c", 1, b = 3)))
## --------<<  standardize_make_procedures_list:2 ends here


