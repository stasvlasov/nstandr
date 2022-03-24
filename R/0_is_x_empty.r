## -------->>  [[file:../harmonizer.src.org::*standardize_empty][standardize_empty:1]]
##' Checks if all elements in vercor(s) are either "", NA, NULL or have zero length
##' @param x input data to check each vector
##' @param return_as_true_if_x_zero_length how to interpret zero lenth input. If TRUE then it returns TRUE. Otherwise NULL.
##' @return logical vector of the same length
standardize_is_data_empty <- function(x
                                  , return_as_true_if_x_zero_length = FALSE) {
    if(length(x) == 0) {
        if(return_as_true_if_x_zero_length) {
            return(TRUE)
        } else {
            return(NULL)
        }
    }
    x_list_checks <-
        lapply(x, function(x) {
            if (length(x) == 0) TRUE else all(x == "" | is.na(x))
        })
    unlist(x_list_checks, recursive = FALSE)
}


##' Removes elements that are either "", NA, NULL or have zero length
##' @param x vector
##' @return updated vector with empty elements removed
##' @export
standardize_omit_empty <- function(x) {
    if(length(x) == 0) return(x)
    x[!sapply(standardize_is_data_empty(x), isTRUE)]
}



## eval things if x empty otherwise return x
standardize_eval_if_empty <- function(x, ..., env = parent.frame()) {
  if(standardize_is_data_empty(x))
    eval(..., envir = env)
  else x
}
## --------<<  standardize_empty:1 ends here


