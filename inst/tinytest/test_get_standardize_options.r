## -------->>  [[file:../../nstandr.src.org::*get_standardize_options][get_standardize_options:2]]
## hack to make functions available in the environment
standardize_options <- nstandr:::standardize_options
## I do not test get_standardize_options directly (and I will not use it in general) because it referes to the parent environment where internal functions are unknown

expect_equal(nstandr:::get_dots(standardize_options
                                 , search_calls_with_formals = c("x", "...")
                                 , search_depth = 5L
                                 , search_up_to_call = c("standardize", "nstandr::standardize")
                                 , skip_checks_for_parent_call = FALSE)
           , as.list(formals("standardize_options")))

standardize_test <- function (x, ...) nstandr:::get_dots(standardize_options
                                                        , search_calls_with_formals = c("x", "...")
                                                        , search_depth = 5L
                                                        , search_up_to_call = c("standardize", "nstandr::standardize")
                                                        , skip_checks_for_parent_call = FALSE)

## should update defauls
expect_equal(standardize_test()$col, 1)
expect_equal(standardize_test(col = 2)$col, 2)

standardize_test <- \(x, col = 2, ...) nstandr:::get_dots(standardize_options
                                 , search_calls_with_formals = c("x", "...")
                                 , search_depth = 5L
                                 , search_up_to_call = c("standardize", "nstandr::standardize")
                                 , skip_checks_for_parent_call = FALSE)

expect_equal(standardize_test()$col, 2)
expect_equal(standardize_test(col = 3)$col, 3)

## Condkitioning on checking arguments (formals) in calls 

## should not update defauls (with skip_checks_for_parent_call = TRUE)
standardize_test_1 <- \() nstandr:::get_dots(standardize_options
                                 , search_calls_with_formals = c("x", "...")
                                 , search_depth = 5L
                                 , search_up_to_call = c("standardize", "nstandr::standardize")
                                 , skip_checks_for_parent_call = FALSE)
standardize_test_2 <- \(x, col = 123) standardize_test_1()
expect_equal(standardize_test_2()$col, 1)

standardize_test_1 <- \() nstandr:::get_dots(standardize_options
                                 , search_calls_with_formals = c("x", "...")
                                 , search_depth = 5L
                                 , search_up_to_call = c("standardize", "nstandr::standardize")
                                 , skip_checks_for_parent_call = FALSE)
standardize_test_2 <- \(col = 123, ...) standardize_test_1()
expect_equal(standardize_test_2()$col, 1)


## should not update defauls (with skip_checks_for_parent_call = FALSE)
standardize_test <- \(x, col = 123) nstandr:::get_dots(standardize_options
                                 , search_calls_with_formals = c("x", "...")
                                 , search_depth = 5L
                                 , search_up_to_call = c("standardize", "nstandr::standardize")
                                 , skip_checks_for_parent_call = FALSE)
expect_equal(standardize_test()$col, 1)

standardize_test <- \(col = 123, ...) nstandr:::get_dots(standardize_options
                                 , search_calls_with_formals = c("x", "...")
                                 , search_depth = 5L
                                 , search_up_to_call = c("standardize", "nstandr::standardize")
                                 , skip_checks_for_parent_call = FALSE)
expect_equal(standardize_test()$col, 1)




## should update defauls
standardize_test <- \(x, col = 123, ...) {
    standardize_ <- \(x, ...) nstandr:::get_dots(standardize_options
                                 , search_calls_with_formals = c("x", "...")
                                 , search_depth = 5L
                                 , search_up_to_call = c("standardize", "nstandr::standardize")
                                 , skip_checks_for_parent_call = FALSE)
    standardize_() 
}
expect_equal(standardize_test()$col, 123)

## should not update defauls
standardize_test <- \(x, col = 123, ...) {
    standardize <- \(x, ...) nstandr:::get_dots(standardize_options
                                 , search_calls_with_formals = c("x", "...")
                                 , search_depth = 5L
                                 , search_up_to_call = c("standardize", "nstandr::standardize")
                                 , skip_checks_for_parent_call = FALSE)
    standardize() 
}
expect_equal(standardize_test()$col, 1)
## --------<<  get_standardize_options:2 ends here


