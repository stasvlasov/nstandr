## -------->>  [[file:../harmonizer.src.org::*harmonize.is.ok][harmonize.is.ok:1]]
##' Checks if a column(s) name/number is valid.
##' 
##' @param col column name/number or vector of columns name/number
##' @param x table
##' @param required is set NULL is not allowed. Default is FALSE.
##' @param allow.negative If `col` is used for negation. Default is FALSE.
##' @param allow.zero Allow `col` to be 0. Default is FALSE.
##' @param several.ok If set `col` should refer to one column. Default is FALSE.
##' @param arg.name Name to use when reporting errors. By default set as `deparse(substitute(col))`
##' @return TRUE if `col` value is ok and FALSE if it is NULL
##' 
##' @md 
harmonize_is_ok_col <- function(col, x
                              , required = FALSE
                              , allow.negative = FALSE
                              , allow.zero = FALSE
                              , several.ok = FALSE
                              , ban.values = NULL
                              , arg.name = deparse(substitute(col))) {
    x.names <- if(is.atomic(x)) attr(x, "name", TRUE) else names(x)
    if(length(col) > 1)
        if(!several.ok) stop("'", arg.name, "' should be single column")
        else all(sapply(col, harmonize_is_ok_col
                      , x, required
                      , allow.negative = ifelse(allow.negative
                                              , all(col < 0)
                                              , FALSE)
                      , allow.zero = allow.zero
                      , arg.name = arg.name
                      , ban.values = ban.values
                      , several.ok = FALSE))
    else if(is.null(col))
        if(required) stop("'", arg.name, "' is required.")
        else FALSE
    else if(length(col) != 1) stop("'", arg.name, "' should be of length 1.")
    else if(col %in% ban.values)
        stop("'", arg.name, "' is not allowed to be: "
           , paste(ban.values, collapse = ", "))
    else if(is.numeric(col) & !is.na(col))
        if(!allow.negative & col < 0) stop("'", arg.name, "' can not be negartive number or mixed.")
        else if(allow.zero & col == 0) TRUE
        else if(abs(col) %in% 1:harmonize_data_width(x)) TRUE
        else stop("'", arg.name, "' number is out of range. Check ncol(x).")
    else if(is.character(col))
        if(col %in% x.names) TRUE
        else stop("'", arg.name, "' name is out of range. Check names(x).")
    else stop("'", arg.name, "' should be ethier numeric or character.")
}




##' Checks if object is valid type and length.
##' 
##' @param x Object to check.
##' @param x.length Length the object should adhere to. Default is objects length so it will always adhere.
##' @param type Type of the object. Default is "logical". If several types are provided that means that it cheches if the x is of either of types! (basically OR function)
##' @param allow.na Is NA allowed? Default is TRUE.
##' @param allow.null Is NULL allowed? Default is TRUE.
##' @param arg.name Name to use when reporting errors. By default set as `deparse(substitute(x))`
##' @return TRUE if type is match, FALSE if x is NULL and it is allowed. Through an error otherwise
##' 
##' @md 
harmonize_is_ok_type <- function(x
                               , x.length = length(x)
                               , type = c("logical"
                                        , "character"
                                        , "numeric"
                                        , "list"
                                        , "atomic"
                                        , NA)
                               , allow.na = TRUE
                               , allow.null = TRUE
                               , arg.name = deparse(substitute(x))) {
    ## if type is missing then assume checking "toggle" argument (TRUE/FALSE)
    if(missing(type)) {
        type <- match.arg(type)
        if(missing(allow.na)) allow.na <- FALSE
        if(missing(allow.null)) allow.null <- FALSE
        if(missing(x.length)) x.length <- 1
    } else {
        type <- match.arg(type, several.ok = TRUE)
    }
    if(allow.null & (length(x) == 0)) return(FALSE)
    else if(!(length(x) %in% x.length))
        stop("Parameter '" ,arg.name, "' has length of ", length(x), " but should be of ", x.length)
    if(allow.na & all(is.na(x))) return(TRUE)
    else if(!allow.na & any(is.na(x))) 
        stop("NAs are not allowed in parameter '", arg.name, "'")
    ## Check types
    if(any(class(x) %in% type)) return(TRUE)
    if(("atomic" %in% type) && is.atomic(x)) return(TRUE)
    # need to check numerics separately because of integers and doubles
    if(("numeric" %in% type) && is.numeric(x)) return(TRUE)
    if(any(is.na(type))) return(TRUE)
    stop("Parameter '", arg.name, "' is type of ", class(x), " but should be one of ", type)
}

##' Checks if ... (dots) arguments are valid.
##' 
##' @param dots.names Character vector of names of ... (dots) arguments. Usually obtained with `names(as.list(...))`.
##' @param formals Character vector of names to match dots agains. Usually obtained with `names(formals(function_name))`.
##' @return TRUE if arguments are ok. FALSE if no arguments are provided (NULL or list())
##' 
##' @md 
harmonize_is_ok_dots <- function(dots.names, formals) {
    if(harmonize_is_ok_type(dots.names
                          , type = "character"
                          , allow.na = FALSE)) {
        if(any(duplicated(dots.names))) {
            stop("Same name arguments used in ... (dots).")
        }
        is.in.formals <- function(name) {
            ifelse(name %in% formals
                 , TRUE
                 , stop("'", name, "' is not in '"
                      , paste(formals, collapse = ", "), "'"))
        }
        all(sapply(dots.names, is.in.formals))
    } else FALSE
}
## --------<<  harmonize.is.ok:1 ends here

