## -------->>  [[file:../nstandr.src.org::*defactor][defactor:1]]
##' Converts factor to character
##' @param x a vector
##' @param check.numeric check if vector is numeric. Default is TRUE. Takes longer with this check but avoids type conversion (numeric to character).
##' @return character vector
defactor_vector <- function(x, check.numeric = FALSE) {
  if(is.factor(x) & check.numeric) {
    levs <- levels(x)
    ## check if levels are numeric (longer)
    ## https://stackoverflow.com/questions/3418128
    if(suppressWarnings(identical(levs
                                , as.character(as.numeric(levs)))))
      as.numeric(levs)[x]
    else
      levs[x]
  }
  else if(is.factor(x))
    levels(x)[x]
  else x
}


##' Defactor the object
##' 
##' Returns object of the same type without factors
##'
##' @param x an object
##' @param conv2dt What to convert to data.table
##' @param x_atomic_name Name to use as a col name if x is atomic
##' @inheritDotParams defactor_vector
##' @return object of the same type without factors
##' @import data.table
##' @export
defactor <- function(x
                   , conv2dt = c("only.tables"
                               , "all.but.atomic"
                               , "all.but.lists"
                               , "all"
                               , "none")
                   , x_atomic_name = NULL
                   , ...) {
  conv2dt <-  match.arg(conv2dt)
  if(is.atomic(x)) {
      if(conv2dt %in% c("only.tables", "all.but.atomic", "none")) {
          defactor_vector(x, ...)
      } else {
          x <- data.table(defactor_vector(x, ...))
          if(!is.null(x_atomic_name)) names(x) <- x_atomic_name
          return(x)
      }
  } else if(class(x)[1] == "list")
      if((conv2dt %in% c("only.tables", "all.but.lists", "none")))
          lapply(x, defactor, conv2dt = "none", ...)
      else
          data.table(lapply(x, defactor, conv2dt = "none", ...))
  else if(conv2dt != "none")
    as.data.table(lapply(x, defactor_vector, ...))
  else if(is.matrix(x))
    as.matrix(lapply(x, defactor_vector, ...))
  else if(is.data.table(x))
    as.data.table(lapply(x, defactor_vector, ...))
  else if(is.data.frame(x))
    as.data.frame(lapply(x, defactor_vector, ...)
                , stringsAsFactors = FALSE)
  else x
}
## --------<<  defactor:1 ends here


