## -------->>  [[file:../harmonizer.src.org::*harmonize.x.length and width][harmonize.x.length and width:1]]
##' Gets lengths of the object
##'
##' @param data input data (table)
##' @return Length (`nrow`) of the object. If it is atomic it returns its length.
##' @export
harmonize_data_length <- function(data) {
   if (is.atomic(data)) length(data) else nrow(data)
}


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
##' @param data object (table)
##' @return Width (ncol) of the object. If it is atomic it is 1.
##' @export
harmonize_data_width <- function(data) {
   if (is.atomic(data)) 1 else ncol(data)
}


x_width <- function(x) {
    if (is.atomic(x)) {
        return(1)
    } else {
        return(ncol(x))
    }
}
## --------<<  harmonize.x.length and width:1 ends here


