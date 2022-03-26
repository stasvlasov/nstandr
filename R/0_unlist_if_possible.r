## -------->>  [[file:../nstandr.src.org::*unlist_if_possible][unlist_if_possible:1]]
##' If column in the `x` table is list unlist it if possible
##' @param x object
##' @param replace_zero_length_with Default is replace NULLs with NA_character_ because vector of just NA is a logical class
##' @param remove_empty_values remove NA, "", etc. from list elements.  (see [standardize_omit_empty()])
##' @return updated object
##' @export
unlist_if_possible <- function(x
                             , replace_zero_length_with = NA_character_
                             , remove_empty_values = TRUE) {
    if(is.list(x)) {
        if(remove_empty_values) {
            x <- lapply(x, standardize_omit_empty)
        }
        len <- sapply(x, length)
        if(all(len == 1)) {
            unlist(x, recursive = FALSE, use.names = FALSE)
        } else if(all(len %in% 0:1)) {
            x[len == 0] <- replace_zero_length_with
            unlist(x, recursive = FALSE, use.names = FALSE)
        } else {
            return(x)
        }
    } else {
        ## assume that x is atomic
        return(x)
    }
}
## --------<<  unlist_if_possible:1 ends here


