## -------->>  [[file:../../nstandr.src.org::*make_alias][make_alias:4]]
make_alias <- nstandr:::make_alias

a <- function(x = 1, y = 1) {
    return(x + y)
}

b <- make_alias(a
              , x = 9
              , .title = "hello world")

expect_equal(b
           , structure(function (x = 9, y = 1) 
           {
               return(x + y)
           }
         , nstandr_procedure_title = "hello world"
         , nstandr_procedure_alias = "a"))
## --------<<  make_alias:4 ends here


