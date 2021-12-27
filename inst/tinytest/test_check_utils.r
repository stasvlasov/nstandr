## -------->>  [[file:../../harmonizer.src.org::*check_harmonize_options][check_harmonize_options:3]]
## test col checks
check_col <- harmonizer:::check_col

fun <- function(col, x) {
    check_col(col, x)
    return(x[[col]])
}


expect_error(fun(list("5"), c("1" = "1","3" = 5)))
expect_error(fun("5", c("1" = "1","3" = 5)))
expect_error(fun(4, c("1" = "1","3" = 5)))
expect_error(fun(c(1,2), c("1" = "1","3" = 5)))
expect_equal(fun(2, c("1" = "1","3" = 5)), "5")





## test rows check
check_rows <- harmonizer:::check_rows

fun <- function(rows, x) {
    check_rows(rows, x)
    return(x[rows])
}

expect_error(fun(list("5"), c("1" = "1","3" = 5)))
expect_error(fun("5", c("1" = "1","3" = 5)))
expect_error(fun(4, c("1" = "1","3" = 5)))
expect_error(fun(c(2,2), c("1" = "1","3" = 5))) # duplicates
expect_equal(fun(c(1,2), c("1" = "1","3" = 5)), c("1" = "1","3" = 5))
expect_equal(fun(2, c("1" = "1","3" = 5)), c("3" = "5"))







## test x checks
check_x <- harmonizer:::check_x

expect_error(check_x(list(1,2,3)))
expect_error(check_x(1))
expect_equal(check_x(c("1", 2, 3)), TRUE)


## ## test
## test.col <- 2
## harmonize.is.ok.col(test.col, data.frame(nu = 1:5, NA, drink = rep("coffee", 5)))
## test.col <- 4
## harmonize.is.ok.col(test.col, data.frame(nu = 1:5, NA, drink = rep("coffee", 5)))
## test.col <- "drink"
## harmonize.is.ok.col(test.col, data.frame(nu = 1:5, NA, drink = rep("coffee", 5)))
## test.col <- "food"
## harmonize.is.ok.col(test.col, data.frame(nu = 1:5, NA, drink = rep("coffee", 5)))
## test.col <- NA
## harmonize.is.ok.col(test.col, data.frame(nu = 1:5, NA, drink = rep("coffee", 5)))
## test.col <- NULL
## harmonize.is.ok.col(test.col, data.frame(nu = 1:5, NA, drink = rep("coffee", 5)))
## test.col <- NULL
## harmonize.is.ok.col(test.col, data.frame(nu = 1:5, NA, drink = rep("coffee", 5)), required = TRUE)
## test.col <- c("nu", "coffee")
## harmonize.is.ok.col(test.col, data.frame(nu = 1:5, NA, drink = rep("coffee", 5)), required = TRUE)
## test.col <- c(1,2)
## harmonize.is.ok.col(test.col, data.frame(nu = 1:5, NA, drink = rep("coffee", 5)), required = TRUE)
## test.col <- c(1,3,0)
## harmonize.is.ok.col(test.col,  data.frame(nu = 1:5, NA, drink = rep("coffee", 5)), required = TRUE, allow.negative = TRUE, allow.zero = TRUE)
## harmonize.is.ok.col(test.col,  data.frame(nu = 1:5, NA, drink = rep("coffee", 5)), required = TRUE, allow.negative = TRUE, allow.zero = TRUE, several.ok = FALSE)
## test.col <- -c(1,2)
## test.col <- c(1,-2)
## harmonize.is.ok.col(test.col,  data.frame(nu = 1:5, NA, drink = rep("coffee", 5)), required = TRUE, allow.negative = TRUE, allow.zero = TRUE)

## test.col <- c(1,3)
## harmonize.is.ok.col(test.col,  data.frame(nu = 1:5, NA, drink = rep("coffee", 5)), required = TRUE, allow.negative = TRUE, allow.zero = TRUE, ban.values = c(3,4,5))




## test
## test.arg <- FALSE
## harmonize.is.ok.type(test.arg)
## test.arg <- c(1,2,3,4,NA)
## harmonize.is.ok.type(test.arg)
## test.arg <- c(1,2,3,4,NA)
## harmonize.is.ok.type(test.arg, type = "numeric")
## test.arg <- c(T,T,F,T,NA)
## harmonize.is.ok.type(test.arg)
## test.arg <- c(NA, NA)
## harmonize.is.ok.type(test.arg, type = "numeric")
## test.arg <- NULL
## harmonize.is.ok.type(test.arg, type = "numeric")
## test.arg <- NA
## harmonize.is.ok.type(test.arg)
## test.arg <- list(1,2,3,NULL)
## harmonize.is.ok.type(test.arg, type = "list")
## test.arg <- list(1,2,3,NULL)
## harmonize.is.ok.type(test.arg, type = c("list", "numeric"))




## ## test
## harmonize.is.ok.dots(names(list(x.col = 4, x.col.update = FALSE))
##                      , names(formals("harmonize.x"))[-c(1:2)] )

## harmonize.is.ok.dots(names(list())
##                      , names(formals("harmonize.x"))[-c(1:2)] )

## harmonize.is.ok.dots(c(NA, NA, 1)
##                      , names(formals("harmonize.x"))[-c(1:2)] )

## harmonize.is.ok.dots(NULL
##                      , names(formals("harmonize.x"))[-c(1:2)] )
## --------<<  check_harmonize_options:3 ends here


