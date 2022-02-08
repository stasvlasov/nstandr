## -------->>  [[file:../harmonizer.src.org::*harmonize_options][harmonize_options:1]]
##' Does nothing but stores (as its own default arguments) options that control vector handeling through harmonization process. These options are available in most harmonizer functions that accept `...` parameter.
##' 
##' @param col Column of interest (the one we need to harmonize) in the `x` object (if it is data.frame like).
##' @param rows Logical vector to filter records of interest. Default is NULL which means do not filter records.
##' @param omitted_rows_value If `rows` parameter is set then merge `omitted_rows_value` with the results (filtered by `rows`). Either single string or a character vector of length `nrow(x)`. If NULL (the default) then original values of `col` are merged with results.
##' @param output_placement Where to inset retults (harmonized vector) in the `x` object. Default options is 'replace_col' which overwrides the `col` in `x` with results. Other options:
##' - 'omit' :: do not write results back to table (usually used when `append_output_copy` is set for temporary values)
##' - 'prepend_to_col' :: prepend to `col`
##' - 'append_to_col' :: append to `col`
##' - 'prepend_to_x' :: prepend to `x` data.frame like object
##' - 'append_to_x' :: append to `x` data.frame like object
##' @param x_atomic_name If `x` is vector use this name for original column if it is in results. Default is "x". If `x` is table the name of `col` will be used.
##' @param output_col_name Use this name for the column with results (harmonized values). Parts in curly brakeds are substitute strings. Options for substitutions are:
##' @eval format_col_name(return_docs = TRUE)
##' @param append_output_copy Whether to append a copy of result vector to `x` object
##' @param output_copy_col_name How the append copy wiil be named
##' 
##' @return It does nothing. Returns NULL silently.
harmonize_options <- function(col = 1
                            , rows = NULL
                            , omitted_rows_value = NULL
                            , output_placement = "replace_col"
                            , name = NULL
                            , x_atomic_name = "x"
                            , output_col_name = "std{_col_name}"
                            , append_output_copy = FALSE
                            , output_copy_col_name = "std{_col_name}_copy{_procedure_index}{_procedure_name}") {
      ## do nothing
      invisible()
  }
## --------<<  harmonize_options:1 ends here


