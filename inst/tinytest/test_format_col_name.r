## -------->>  [[file:../../nstandr.src.org::*get_target & inset_target][get_target & inset_target:5]]
format_col_name <- nstandr:::format_col_name

## format_col_name(return_docs = TRUE)

expect_equal(format_col_name("lala")
           , "lala")

expect_equal(format_col_name("lala", "a")
           , "lala")

expect_equal(format_col_name("lala{_col_name}", "a")
           , "lala_a")

expect_equal(format_col_name("lala{_col_name}{_index_suffix}", "a")
           , "lala_a")

expect_equal(format_col_name("lala{_col_name}{_index_suffix}", "a", "lala")
           , "lala_a")

expect_equal(format_col_name("lala{_col_name}{_index_suffix}", "a", "lala_a")
           , "lala_a_1")

expect_equal(format_col_name("lala{_col_name}{_index_suffix}", "a", c("lala_a", "lala_a_41"))
           , "lala_a_42")

expect_equal({
    nstandr_standardize_procedure_index <- 6
    nstandr_standardize_procedure_name <- "nber_replacement"
    "{col_name_}harmonizing{_procedure_index}" |>
        paste0("{_procedure_name}") |>
        format_col_name("col")
}
, "col_harmonizing_6_nber_replacement")
## --------<<  get_target & inset_target:5 ends here


