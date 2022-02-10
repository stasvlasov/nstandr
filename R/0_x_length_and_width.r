## -------->>  [[file:../harmonizer.src.org::*x_length and x_width][x_length and x_width:1]]
##' Gets lengths of the object
##'
##' @param x input data (table)
##' @return Length (`nrow`) of the object. If it is atomic it returns its length.
##' @export
x_length <- function(x) {
    if (is.atomic(x) || is.null(x)) {
        length(x)
    } else if(is.list(x)) {
        length(x[[1]])
    } else {
        nrow(x)
    }
}




##' Gets width of the object
##'
##' @param x object (table)
##' @return Width (ncol) of the object. If it is atomic it is 1.
##' @export
x_width <- function(x) {
    if (is.atomic(x)) {
        return(1)
    } else {
        return(ncol(x))
    }
}
## --------<<  x_length and x_width:1 ends here


