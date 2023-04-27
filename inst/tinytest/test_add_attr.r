## -------->>  [[file:../../nstandr.src.org::*make_alias][make_alias:3]]
add_attr <- nstandr:::add_attr

a <- function(x = 1, y = 1) {
    return(x + y)
}

add_attr(a
       , .title = "lala"
       , .description = "lala")

expect_equal(a
           , structure(function (x = 1, y = 1) 
           {
               return(x + y)
           }, nstandr_procedure_title = "lala", nstandr_procedure_description = "lala"))
## --------<<  make_alias:3 ends here


