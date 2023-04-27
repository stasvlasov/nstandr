## -------->>  [[file:../../nstandr.src.org::*get_target & inset_target][get_target & inset_target:6]]
infer_moving_target_from_names <- nstandr:::infer_moving_target_from_names

expect_equal(infer_moving_target_from_names(x_names = c("x", "std_x")
                                          , col = 1
                                          , output_col_name = "std{_col_name}"
                                          , output_placement = "append_to_col")
           , 2L)



expect_null(infer_moving_target_from_names(x_names = c("name", "codes", "lala")
                                         , col = 1
                                         , output_col_name = "codes.new"
                                         , output_placement = "append_to_col"
                                         , return_null_for_new_col = TRUE))
## --------<<  get_target & inset_target:6 ends here


