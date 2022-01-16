## -------->>  [[file:../harmonizer.src.org::*harmonize_options][harmonize_options:1]]
##' Does nothing but stores (as its own default arguments) options that control vector handeling through harmonization process. These options are available in most harmonizer functions that accept `...` parameter.
##' 
##' @param col Column of interest (the one we need to harmonize) in the `x` object (if it is data.frame like).
##' @param rows Logical vector to filter records of interest. Default is NULL which means do not filter records.
##' @param omitted_rows_values If `rows` parameter is set then merge `omitted_rows_values` with the results (filtered by `rows`). Either single string or a character vector of length `nrow(x)`. If NULL (the default) then original values of `col` are merged with results.
##' @param placement Where to inset retults (harmonized vector) in the `x` object. Default options is 'replace_col' which overwrides the `col` in `x` with results. Other options:
##' - 'omit' :: do not write results back to table (usually used when `append_copy` is set for temporary values)
##' - 'prepend_to_col' :: prepend to `col`
##' - 'append_to_col' :: append to `col`
##' - 'prepend_to_x' :: prepend to `x` data.frame like object
##' - 'append_to_x' :: append to `x` data.frame like object
##' @param name Use this name for the column with results (harmonized values). Default is NA, which means that either `name_for_x_atomic` if `x` is vector or original col name will be used with `name_suffix` at the end.
##' @param name_for_x_atomic If `x` is vector use this name for original column if it is in results. Default is "x". If `x` is table the name of `col` will be used.
##' @param name_suffix If `name` is not set the use this as suffix (default is "harmonized"). If the name with the same suffix already exists in `select_x_cols` it will add counter at the end to avoid variables with the same names.
##' @param append_copy Whether to append a copy of result vector to `x` object
##' @param append_copy_name_format How the append copy wiil be named
##' @param select_cols If x` object is table, set the columns to cbind to the result table. Default is cbind all but the original (unharmonized) column (col).
##' 
##' @return Always NULL. It does nothing.
harmonize_options <- function(col = 1
                            , rows = NULL
                            , omitted_rows_values = NULL
                            , placement = "replace_col"
                            , name = NULL
                            , name_for_x_atomic = "x"
                            , name_suffix = "_harmonized"
                            , append_copy = FALSE
                            , append_copy_name_format = "%name_harmonizing_%number_%procedure"
                            , select_cols = NULL) {
    ## do nothing
    return()
}
## --------<<  harmonize_options:1 ends here


