## -------->>  [[file:../nstandr.src.org::*standardize_empty][standardize_empty:1]]
##' Checks if all elements in vector(s) are either "", NA, NULL or have zero length
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
    if(is.atomic(x)) {
        return(is.na(x) | x == "")
    } else if(is.list(x)) {
        ## try to speed things up
        x_empty <- rep(FALSE, length(x))
        ## check for NULLs
        x_list_length <- sapply(x, length)
        if(any(x_list_length == 0)) {
            x_empty[x_list_length == 0] <- TRUE
        }
        ## check single elements
        if(any(x_list_length == 1)) {
            x_empty_singles <- x[x_list_length == 1] |> unlist()
            x_empty_singles <- is.na(x_empty_singles) | x_empty_singles == ""
            x_empty[x_list_length == 1] <- x_empty_singles
        }
        ## check lengthy elements
        if(any(x_list_length > 1)) {
            x_empty_multiples <- x[x_list_length > 1] |> sapply(\(.x) all(is.na(.x) | .x == ""))
            x_empty[x_list_length > 1] <- x_empty_multiples
        }
        return(x_empty)
    }
}


##' Removes elements that are either "", NA, NULL or have zero length
##' @param x vector
##' @return updated vector with empty elements removed
##' @export
standardize_omit_empty <- function(x) {
    if(length(x) == 0) return(x)
    x[!standardize_is_data_empty(x)]
}



## eval things if x empty otherwise return x
standardize_eval_if_empty <- function(x, ..., env = parent.frame()) {
  if(standardize_is_data_empty(x))
    eval(..., envir = env)
  else x
}
## --------<<  standardize_empty:1 ends here


