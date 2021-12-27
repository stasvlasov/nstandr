## -------->>  [[file:../../harmonizer.src.org::*get_harmonize_options][get_harmonize_options:2]]
## hack to make functions available in the environment
harmonize_options <- harmonizer:::harmonize_options
## I do not test get_harmonize_options directly (and I will not use it in general) because it referes to the parent environment where internal functions are unknown

expect_equal(harmonizer:::get_dots(harmonize_options
                                 , search_while_calls_have_formals = c("x", "...")
                                 , search_up_nframes = 5L
                                 , search_up_to_call = c("harmonize", "harmonizer::harmonize")
                                 , skip_checks_for_parent_call = FALSE)
           , as.list(formals("harmonize_options")))

harmonize_test <- function (x, ...) harmonizer:::get_dots(harmonize_options
                                                        , search_while_calls_have_formals = c("x", "...")
                                                        , search_up_nframes = 5L
                                                        , search_up_to_call = c("harmonize", "harmonizer::harmonize")
                                                        , skip_checks_for_parent_call = FALSE)

## should update defauls
expect_equal(harmonize_test()$col, 1)
expect_equal(harmonize_test(col = 2)$col, 2)

harmonize_test <- \(x, col = 2, ...) harmonizer:::get_dots(harmonize_options
                                 , search_while_calls_have_formals = c("x", "...")
                                 , search_up_nframes = 5L
                                 , search_up_to_call = c("harmonize", "harmonizer::harmonize")
                                 , skip_checks_for_parent_call = FALSE)

expect_equal(harmonize_test()$col, 2)
expect_equal(harmonize_test(col = 3)$col, 3)

## Condkitioning on checking arguments (formals) in calls 

## should not update defauls (with skip_checks_for_parent_call = TRUE)
harmonize_test_1 <- \() harmonizer:::get_dots(harmonize_options
                                 , search_while_calls_have_formals = c("x", "...")
                                 , search_up_nframes = 5L
                                 , search_up_to_call = c("harmonize", "harmonizer::harmonize")
                                 , skip_checks_for_parent_call = FALSE)
harmonize_test_2 <- \(x, col = 123) harmonize_test_1()
expect_equal(harmonize_test_2()$col, 1)

harmonize_test_1 <- \() harmonizer:::get_dots(harmonize_options
                                 , search_while_calls_have_formals = c("x", "...")
                                 , search_up_nframes = 5L
                                 , search_up_to_call = c("harmonize", "harmonizer::harmonize")
                                 , skip_checks_for_parent_call = FALSE)
harmonize_test_2 <- \(col = 123, ...) harmonize_test_1()
expect_equal(harmonize_test_2()$col, 1)


## should not update defauls (with skip_checks_for_parent_call = FALSE)
harmonize_test <- \(x, col = 123) harmonizer:::get_dots(harmonize_options
                                 , search_while_calls_have_formals = c("x", "...")
                                 , search_up_nframes = 5L
                                 , search_up_to_call = c("harmonize", "harmonizer::harmonize")
                                 , skip_checks_for_parent_call = FALSE)
expect_equal(harmonize_test()$col, 1)

harmonize_test <- \(col = 123, ...) harmonizer:::get_dots(harmonize_options
                                 , search_while_calls_have_formals = c("x", "...")
                                 , search_up_nframes = 5L
                                 , search_up_to_call = c("harmonize", "harmonizer::harmonize")
                                 , skip_checks_for_parent_call = FALSE)
expect_equal(harmonize_test()$col, 1)




## should update defauls
harmonize_test <- \(x, col = 123, ...) {
    harmonize_ <- \(x, ...) harmonizer:::get_dots(harmonize_options
                                 , search_while_calls_have_formals = c("x", "...")
                                 , search_up_nframes = 5L
                                 , search_up_to_call = c("harmonize", "harmonizer::harmonize")
                                 , skip_checks_for_parent_call = FALSE)
    harmonize_() 
}
expect_equal(harmonize_test()$col, 123)

## should not update defauls
harmonize_test <- \(x, col = 123, ...) {
    harmonize <- \(x, ...) harmonizer:::get_dots(harmonize_options
                                 , search_while_calls_have_formals = c("x", "...")
                                 , search_up_nframes = 5L
                                 , search_up_to_call = c("harmonize", "harmonizer::harmonize")
                                 , skip_checks_for_parent_call = FALSE)
    harmonize() 
}
expect_equal(harmonize_test()$col, 1)
## --------<<  get_harmonize_options:2 ends here


