## -------->>  [[file:../../harmonizer.src.org::*get_vector & inset_vector][get_vector & inset_vector:2]]
format_append_copy <- harmonizer:::format_append_copy

expect_equal({
    harmonizer_harmonize_procedure_number <- 6
    harmonizer_harmonize_procedure_name <- "nber_replacement"
    "{col_name_}harmonizing{_procedure_number}{_procedure_name}" |>
        format_append_copy("col")
}
, "col_harmonizing_6_nber_replacement")
## --------<<  get_vector & inset_vector:2 ends here


