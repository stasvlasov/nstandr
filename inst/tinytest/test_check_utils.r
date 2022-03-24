## -------->>  [[file:../../harmonizer.src.org::*check_standardize_options][check_standardize_options:3]]
## test col checks
check_col <- nstandr:::check_col

fun <- function(col, x) {
    check_col(col, x)
    return(x[[col]])
}


expect_error(fun(list("5"), c("1" = "1","3" = 5)))
expect_error(fun("5", c("1" = "1","3" = 5)))
expect_error(fun(4, c("1" = "1","3" = 5)))
expect_error(fun(c(1,2), c("1" = "1","3" = 5)))
expect_equal(fun(1, c("1" = "1","3" = 5)), "1")

expect_error(fun(3, data.frame(a = 1, b = 2)))
expect_equal(fun(2, data.frame(a = 1, b = 2)), 2)

## test rows check
check_rows <- nstandr:::check_rows

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
check_x <- nstandr:::check_x

expect_error(check_x(list(1,2,3)))
expect_error(check_x(1))
expect_equal(check_x(c("1", 2, 3)), TRUE)


## ## test
## test.col <- 2
## standardize.is.ok.col(test.col, data.frame(nu = 1:5, NA, drink = rep("coffee", 5)))
## test.col <- 4
## standardize.is.ok.col(test.col, data.frame(nu = 1:5, NA, drink = rep("coffee", 5)))
## test.col <- "drink"
## standardize.is.ok.col(test.col, data.frame(nu = 1:5, NA, drink = rep("coffee", 5)))
## test.col <- "food"
## standardize.is.ok.col(test.col, data.frame(nu = 1:5, NA, drink = rep("coffee", 5)))
## test.col <- NA
## standardize.is.ok.col(test.col, data.frame(nu = 1:5, NA, drink = rep("coffee", 5)))
## test.col <- NULL
## standardize.is.ok.col(test.col, data.frame(nu = 1:5, NA, drink = rep("coffee", 5)))
## test.col <- NULL
## standardize.is.ok.col(test.col, data.frame(nu = 1:5, NA, drink = rep("coffee", 5)), required = TRUE)
## test.col <- c("nu", "coffee")
## standardize.is.ok.col(test.col, data.frame(nu = 1:5, NA, drink = rep("coffee", 5)), required = TRUE)
## test.col <- c(1,2)
## standardize.is.ok.col(test.col, data.frame(nu = 1:5, NA, drink = rep("coffee", 5)), required = TRUE)
## test.col <- c(1,3,0)
## standardize.is.ok.col(test.col,  data.frame(nu = 1:5, NA, drink = rep("coffee", 5)), required = TRUE, allow.negative = TRUE, allow.zero = TRUE)
## standardize.is.ok.col(test.col,  data.frame(nu = 1:5, NA, drink = rep("coffee", 5)), required = TRUE, allow.negative = TRUE, allow.zero = TRUE, several.ok = FALSE)
## test.col <- -c(1,2)
## test.col <- c(1,-2)
## standardize.is.ok.col(test.col,  data.frame(nu = 1:5, NA, drink = rep("coffee", 5)), required = TRUE, allow.negative = TRUE, allow.zero = TRUE)

## test.col <- c(1,3)
## standardize.is.ok.col(test.col,  data.frame(nu = 1:5, NA, drink = rep("coffee", 5)), required = TRUE, allow.negative = TRUE, allow.zero = TRUE, ban.values = c(3,4,5))




## test
## test.arg <- FALSE
## standardize.is.ok.type(test.arg)
## test.arg <- c(1,2,3,4,NA)
## standardize.is.ok.type(test.arg)
## test.arg <- c(1,2,3,4,NA)
## standardize.is.ok.type(test.arg, type = "numeric")
## test.arg <- c(T,T,F,T,NA)
## standardize.is.ok.type(test.arg)
## test.arg <- c(NA, NA)
## standardize.is.ok.type(test.arg, type = "numeric")
## test.arg <- NULL
## standardize.is.ok.type(test.arg, type = "numeric")
## test.arg <- NA
## standardize.is.ok.type(test.arg)
## test.arg <- list(1,2,3,NULL)
## standardize.is.ok.type(test.arg, type = "list")
## test.arg <- list(1,2,3,NULL)
## standardize.is.ok.type(test.arg, type = c("list", "numeric"))




## ## test
## standardize.is.ok.dots(names(list(x.col = 4, x.col.update = FALSE))
##                      , names(formals("standardize.x"))[-c(1:2)] )

## standardize.is.ok.dots(names(list())
##                      , names(formals("standardize.x"))[-c(1:2)] )

## standardize.is.ok.dots(c(NA, NA, 1)
##                      , names(formals("standardize.x"))[-c(1:2)] )

## standardize.is.ok.dots(NULL
##                      , names(formals("standardize.x"))[-c(1:2)] )
## --------<<  check_standardize_options:3 ends here


