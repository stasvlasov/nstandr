## -------->>  [[id:org:g5wa69d1ffi0][Add package documentation:1]]
#' @details
#' Harmonizer package standardizes (harmonizes) organizational names
#'     mainly using procedures described in Thoma et al. (2010) and
#'     Magerman, Looy, Bart, & Song (2006) but not only.  This is work
#'     in progress. Please, file an issues or suggestion if you have
#'     any.  The main function is [harmonize()].
#' @keywords internal
"_PACKAGE"
## --------<<  Add package documentation:1 ends here



## -------->>  [[id:org:rixkspb0wei0][harmonize.x.length and width:1]]
##' Gets lengths of the object
##'
##' @param data input data (table)
##' @return Length (`nrow`) of the object. If it is atomic it returns its length.
##' @export
harmonize_data_length <- function(data) {
   if (is.atomic(data)) length(data) else nrow(data)
}

##' Gets width of the object
##'
##' @param data object (table)
##' @return Width (ncol) of the object. If it is atomic it is 1.
##' @export
harmonize_data_width <- function(data) {
   if (is.atomic(data)) 1 else ncol(data)
}
## --------<<  harmonize.x.length and width:1 ends here



## -------->>  [[id:org:3971f8s0lei0][harmonize.empty:1]]
##' Checks if all elements in vercor(s) are either "", NA, NULL or have zero length
##' @param data input data to check each vector
##' @return logical vector of the same length
##' @export
harmonize_is_data_empty <- function(data) {
    data_list_checks <-
        lapply(data, function(x) {
            if (length(x) == 0) TRUE else all(x == "" | is.na(x))
        })
    unlist(data_list_checks, recursive = FALSE)
}


##' Removes elements that are either "", NA, NULL or have zero length
##' @param x vector
##' @return updated vector with empty elements removed
##' @export
harmonize_omit_empty <- function(x) {
  x[!sapply(harmonize_is_data_empty(x), isTRUE)]
}



## eval things if x empty otherwise return x
harmonize_eval_if_empty <- function(x, ..., env = parent.frame()) {
  if(harmonize_is_data_empty(x))
    eval(..., envir = env)
  else x
}
## --------<<  harmonize.empty:1 ends here



## -------->>  [[id:org:uj31f8s0lei0][harmonize.escape.regex:1]]
##' Escapes special for regex characters
##' @param string character vector
##' @return character vector with all special to regex characters escaped
##'
##' @import stringr
##' @export
harmonize_escape_regex <- function(string) str_replace_all(string, "(\\W)", "\\\\\\1")

##' Escapes special for different types of pattern
##' @param string character vector
##' @return character vector with all special to regex characters
##'     escaped
##' @param type whether it should escape regex ("fixed") add beginning
##'     ("begins") or ending ("ends") matcher. Or if value is "regex"
##'     then do not change the string. Also possible to escape a regex
##'     for exact match ("exact") or exact match after trimming spaces
##'     ("trim.exact")
##' @import stringr
##' @export
harmonize_escape_type <- function(string
                                , type = c("fixed"
                                         , "begins"
                                         , "begins.trimmed"
                                         , "ends"
                                         , "ends.trimmed"
                                         , "regex"
                                         , "exact"
                                         , "exact.trimmed")
                                , all.regex = TRUE) {
    type <- match.arg(type)
    if(type == "regex")
        string
    else if(type == "fixed")
        if(all.regex) harmonize_escape_regex(string)
        else string
    else if(type == "begins")
        paste0("^", harmonize_escape_regex(string))
    else if(type == "begins.trimmed")
        paste0("^\\s*", harmonize_escape_regex(string))
    else if(type == "ends")
        paste0(harmonize_escape_regex(string), "$")
    else if(type == "ends.trimmed")
        paste0(harmonize_escape_regex(string), "\\s*$")
    else if(type == "exact")
        if(all.regex) paste0("^", harmonize_escape_regex(string), "$")
        else string
    else if(type == "exact.trimmed")
        if(all.regex)  paste0("^\\s*", harmonize_escape_regex(string), "\\s*$")
        else str_trim(string)
}

##' Escapes special for regex characters conditionally
##' @param strings character vector
##' @param conds character vector of the same length as `strings` with instructions whether to escape regex ("fixed") add beginning ("begins") or ending ("ends") matcher. Or if value is "regex" then do not change the string. Also possible to escape a regex for exact match ("exact") or exact match after trimming spaces ("trim.exact")
##' @param all.regex ......
##' @return string with all special to regex characters escaped
##'
##' @import stringr
harmonize_escape_types <- function(patterns, conds, all.regex = FALSE) {
    if(length(conds) == 1 || length(unique(conds)) == 1) {
        conds %<>% extract(1)
        harmonize_escape_type(patterns, conds, all.regex = all.regex)
    }
    else if(length(conds) == length(patterns))
        mapply(function(pattern, cond) {
            harmonize_escape_type(pattern, cond)
        }
      , patterns
      , conds
      , SIMPLIFY = TRUE)
    else stop("patterns.type misspecified - wrong length!")
}
## --------<<  harmonize.escape.regex:1 ends here



## -------->>  [[id:org:c77b69d1ffi0][harmonize.add.suffix:1]]
##' Adds a suffix to the string and counter at the end if needed
##'
##' @param name Variable name
##' @param suffix Suffix
##' @param x.names Vector of variable names in x to check for duplicates and if we need to add a counter at the end
##' @import magrittr stringr
##'
##' @return Returns a new name
harmonize_add_suffix <- function(name, suffix, x.names
                               , search.suffix.in.name = TRUE
                               , suffix.nbr.init = 1
                               , suffix.nbr = NULL) {
  ## remove suffix from name if it is already there..
  name.base <- if(search.suffix.in.name)
                 str_remove(name, paste0("\\.", suffix, "(\\.\\d+$|$)"))
               else name
  name.with.suffix <- paste0(name.base, ".", suffix)
  name.with.suffix.regex.nbr <-
    paste0("(?<=", harmonize_escape_regex(name.with.suffix), "\\.)", "\\d+$")
  suffix.nbr.init <- if(name.with.suffix %in% x.names)
                       suffix.nbr.init - 1
                     else NULL
  suffix.nbr <-
    c(x.names, ifelse(search.suffix.in.name, name, NULL)) %>% 
    str_extract(name.with.suffix.regex.nbr) %>%
    as.numeric %>%
    {if(all(is.na(.))) suffix.nbr.init
     else max(., na.rm = TRUE)} %>%
    add(1)
  ## return name
  if(length(suffix.nbr) == 0)
    name.with.suffix
  else
    name.with.suffix %>%
      paste0(".", suffix.nbr)
}
## --------<<  harmonize.add.suffix:1 ends here



## -------->>  [[id:org:x3j0f8s0lei0][harmonize.defactor:1]]
##' Converts factor to character
##' @param x a vector
##' @param check.numeric check if vector is numeric. Default is TRUE. Takes longer with this check but avoids type conversion (numeric to character).
##' @return character vector
harmonize_defactor_vector <- function(x, check.numeric = TRUE) {
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
##' @param ... 
##' @inheritDotParams harmonize_defactor_vector
##' @return object of the same type without factors
##'  
##' @import tibble data.table
##' 
##' @export
harmonize_defactor <- function(x
                             , conv2dt = c("only.tables"
                                            , "all.but.atomic"
                                            , "all.but.lists"
                                            , "all"
                                            , "none"), ...) {
  conv2dt <-  match.arg(conv2dt)
  if(is.atomic(x)) {
    if(conv2dt %in% c("only.tables", "all.but.atomic", "none"))
      harmonize_defactor_vector(x, ...)
    else
      data.table(harmonize_defactor_vector(x, ...))
  } else if(class(x)[1] == "list")
    if((conv2dt %in% c("only.tables", "all.but.lists", "none")))
      lapply(x, harmonize_defactor, conv2dt = "none", ...)
    else
      data.table(lapply(x, harmonize_defactor, conv2dt = "none", ...))
  else if(conv2dt != "none")
    as.data.table(lapply(x, harmonize_defactor_vector, ...))
  else if(is.matrix(x))
    as.matrix(lapply(x, harmonize_defactor_vector, ...))
  else if(is.data.table(x))
    as.data.table(lapply(x, harmonize_defactor_vector, ...))
  else if(is_tibble(x))
    as_tibble(lapply(x, harmonize_defactor_vector, ...))
  else if(is.data.frame(x))
    as.data.frame(lapply(x, harmonize_defactor_vector, ...)
                , stringsAsFactors = FALSE)
  else x
}
## --------<<  harmonize.defactor:1 ends here



## -------->>  [[id:org:ld4hpqj01li0][harmonize.is.ok:1]]
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



## -------->>  [[id:org:rjvdj9s0lei0][harmonize.x:1]]
##' Gets a vector to harmonize and puts it back.
##'
##' The function `harmonize.x` basically works as two functions depending whether the second optional parameter `inset.vector` is provided. If `inset.vector` is not provided the function returns a vector that we want to process (harmonize) from object `x` and inset it back to the original object later.  If `inset.vector` (harmonized vector) is provided the function returns updated `x`.
##' 
##' @param x an object
##' @param inset.vector a vector to inset. Optional. Default is NULL
##' @param x.col vector of interest in `x` object
##' @param x.col.update Update values in `x.col` column. Default is FALSE. If set `inset.append`, `inset.name` and `inset.suffix` are ignored. Also if set the default for `return.x.cols.all` will be set to TRUE.
##' @param x.rows Logical vector to filter records of interest. Default is NULL which means do not filter records
##' @param x.atomic.name If `x` is vector use this name for original column if it is in results. Default is "x". If `x` is table the name of `x.col` will be used.
##' @param inset.omitted.val If `x.rows` is set merge these values to the results. It should be a vector of length 1 or `nrow(x)`. If the value is NULL (default) then use values of `x.col`.
##' @param inset.append If set then put `inset.vector` as the last instead of first vector/column. Default is FALSE.
##' @param inset.suffix If `inset.name` is not set the use this as suffix (default is "harmonized"). If the name with the same suffix already exists in `return.x.cols` it will add counter at the end to avoid variables with the same names.
##' @param inset.name Use this name for the first column in results (harmonized names). Default is NULL, which means that either x.atomic.name if x is vector or original x.col name will be used with `inset.suffix` at the end.
##' @param return.x.cols If x is table, set the columns to cbind to the result table. Default is cbind all but the original (unharmonized) column (x.col).
##' @param return.x.cols.all Whether to bind all columns in x. Defaults depends on values of `x.col.update` and `inset.append`. If either is set then defaut values is TRUE otherwise FALSE. If set to TRUE by user the return.x.cols is ignored.
##'
##' @return Vector or data.table
##'
##' @md
##' @import magrittr stringr data.table
##' @export
harmonize.x <- function(x
                      , inset.vector = NULL
                      , x.col = 1
                      , x.col.update = FALSE
                      , x.rows = NULL
                      , x.atomic.name = "x"
                      , inset.omitted.val = NULL
                      , inset.append = FALSE
                      , inset.suffix = "harmonized"
                      , inset.name = NA
                      , return.x.cols =
                          -ifelse(is.numeric(x.col), x.col, match(x.col, names(x)))
                      , return.x.cols.all = inset.append | x.col.update ) {
  ## ------------------------------
  if(is.null(inset.vector)) {
    ## if nothing was provided as x.vector then make and return one
    harmonize.x.check.args()
    harmonize_target_get()
  } else {
    ## if inset.vector is provided put it back to x according to settings
    harmonize.x.check.args()
    harmonize.x.inset.check.args()
    harmonize.x.inset()
  }
}





## functions that only runs within harmonize.x
## --------------------------------------------------------------------------------

## Tests Arguments
harmonize.x.check.args <- function(env = parent.frame()) {
  evalq({
    ## - check x.col
    if(harmonize_is_ok_col(x.col, x, required = TRUE)) {
      x.col %<>% ifelse(is.numeric(.), ., match(., names(x)))
    }
    ## - check x.rows
    if(!harmonize_is_ok_type(x.rows, harmonize_data_length(x), type = "logical")) {
      x.rows <- TRUE  # select all if x.rows NULL 
    }
  }, envir = env)
}


harmonize.x.inset.check.args <- function(env = parent.frame()) {
    evalq({
        ## - check inset.vector
        harmonize_is_ok_type(inset.vector
                           , x.length = if(isTRUE(x.rows)) harmonize_data_length(x)
                                        else sum(x.rows)
                           , type = c("atomic", "list"))
        ## - check inset.omitted.val
        if(!harmonize_is_ok_type(inset.omitted.val
                               , x.length = c(1, harmonize_data_length(x))
                               , type = "atomic")) {
            inset.omitted.val <- harmonize_data_get_col(x, x.col)
        } else if(length(inset.omitted.val) == 1) {
            inset.omitted.val %<>% harmonize_defactor %>% rep(harmonize_data_length(x))
        } else {
            inset.omitted.val %<>% harmonize_defactor
        }
        ## - check return.x.cols
        harmonize_is_ok_type(return.x.cols.all)
        ## return.x.cols.all could be TRUE if inset.append | x.col.update
        ## if return.x.cols.all is not set manually but return.x.cols is
        ## then respect return.x.cols
        if(return.x.cols.all && (!missing(return.x.cols.all) || missing(return.x.cols)))
            ## set return.x.cols to all
            return.x.cols <- 1:harmonize_data_width(x)
        else if(harmonize_is_ok_col(return.x.cols, x
                                  , allow.negative = TRUE
                                  , several.ok = TRUE))
            return.x.cols %<>% switch(is.numeric(.) + 1, match(., names(x)), .)
        else {
            ## set it to zero if it is null
            return.x.cols <- 0
        }
        ## - check inset.append
        harmonize_is_ok_type(inset.append)
        ## - check x.col.update
        harmonize_is_ok_type(x.col.update)
        if(x.col.update &&
           ((all(return.x.cols < 0) && (-x.col %in% return.x.cols)) ||
            (all(return.x.cols >= 0) && !(x.col %in% return.x.cols)))) {
            stop("'x.col.update' is set but 'x.col' is excluded by 'return.x.cols'")
        }
        ## - check names
        if(!x.col.update) {
            harmonize_is_ok_type(inset.name, x.length = 1
                               , type = "character", allow.null = FALSE)
            if(inset.name %in% names(x)[return.x.cols]) {
                stop("The harmonized column name: ", inset.name, " is alredy exists.")
            }
            harmonize_is_ok_type(inset.suffix, x.length = 1
                               , type = "character"
                               , allow.na = FALSE, allow.null = FALSE)
            harmonize_is_ok_type(x.atomic.name, x.length = 1
                               , type = "character"
                               , allow.na = FALSE, allow.null = FALSE)
        }
    }, envir = env)
}

##' Gets a target vector to harmonize.
##'
##' @param data Input data. Can be vector, data.frame or a data.table
##' @param col Column of interest in the input `data`. The vector we would like to work with. This parameter is ignored if input `data` is a vector (checked by `is.atomic`)
##' @param rows Rows of interest
##'
##' @return A vector. Factors in imput `data` are converted to string.
##'
##' @md
harmonize_target_get <- function(data, col, rows) {
    harmonize_data_get_col(data, col)[rows]
}



harmonize_data_get_col <- function(x, col) {
    if(is.atomic(x))
        harmonize_defactor(x)
    else
        harmonize_defactor(x[[col]])
}

## binds to existing table
harmonize.x.inset <- function(env = parent.frame()) {
    evalq({
        ## inset filtered rows (this makes list if inset.vector is list)
        inset.vector %<>% inset(inset.omitted.val, x.rows, .)
        ## inset inset.vector to x
        if(is.atomic(x) & isFALSE(return.x.cols == 1)) {
            inset.vector
        } else if(x.col.update) {
            x %>%
              harmonize_defactor(conv2dt = "all") %>% 
              inset2(x.col, value = inset.vector) %>% 
              extract(., ,return.x.cols, with = FALSE)
        } else if(isTRUE(return.x.cols == 0)) {
            inset.vector
        } else {
          ## set harmonized name
          x.names <- if(is.atomic(x)) x.atomic.name else names(x)
          inset.name %<>%
              harmonize_eval_if_empty(
                  harmonize_add_suffix(x.names[x.col]
                                     , inset.suffix
                                     , x.names[return.x.cols])) %>%
              make.names
          ## (pre)append inset.vector to x
          x %<>% harmonize_defactor(conv2dt = "all") # returns data.table
            inset.vector %>%
                data.table %>%          # should make one column even if inset is list
                set_names(inset.name) %>%
                harmonize.x.cbind(x[, return.x.cols, with = FALSE], inset.append)
        }
    }, envir = env)
}

harmonize.x.cbind <- function(inset.vector, x, append = FALSE) {
  if(isTRUE(append))
    cbind(x, inset.vector)
  else
    cbind(inset.vector, x)
}
## --------<<  harmonize.x:1 ends here



## -------->>  [[id:org:p323mg11m9j0][harmonize_options:1]]
##' Does nothing but stores (as its own default arguments) options that control vector handeling through harmonization process. These options are available in most harmonizer functions that accept `...` parameter.
##' 
##' @param col Column of interest in the `data` object. The one we need to harmonize
##' @param rows Logical vector to filter records of interest. Default is NULL which means do not filter records
##' @param ommitted_rows_values If `rows` is set merge these values to the results. It should be a vector of length 1 or `nrow(data). If the value is NULL (default) then use original values of `col`
##' @param placement Where to inset retults (harmonized vector) in the `data` object. Default options is "replace_col" which overwrides the `col` in `data` with results. 
##' @param name Use this name for the first column in results (harmonized names). Default is NULL, which means that either name_if_data_atomic if data is vector or original col name will be used with `name_suffix` at the end.
##' @param name_if_input_atomic If `data` is vector use this name for original column if it is in results. Default is "data". If `data` is table the name of `col` will be used.
##' @param name_suffix If `name` is not set the use this as suffix (default is "harmonized"). If the name with the same suffix already edataists in `select_data_cols` it will add counter at the end to avoid variables with the same names.
##' @param append_copy Whether to append a copy of result vector to `data` object
##' @param append_copy_name_format How the append copy wiil be named
##' @param select_cols If data` object is table, set the columns to cbind to the result table. Default is cbind all but the original (unharmonized) column (col).
##' 
##' @return Always NULL. It does nothing.
harmonize_options <- function(col = 1
                            , rows = NULL
                            , ommitted_rows_values = NULL
                            , placement = c(
                                  "replace_col"
                                , "prepend_to_data"
                                , "append_to_data"
                                , "prepend_to_col"
                                , "append_to_col"
                                , "ignore")
                            , name = NA
                            , name_if_input_atomic = "names"
                            , name_suffix = "_harmonized"
                            , append_copy = FALSE
                            , append_copy_name_format = "%name_harmonizing_%number_%procedure"
                            , select_cols = NA) {
    ## do nothing
    return()
}
## --------<<  harmonize_options:1 ends here



## -------->>  [[id:org:y3obsm80daj0][get_harmonize_options:1]]
##' Gets `harmonize_options` at point with consistent updates up through calling stack.
##'
##' Limited to max stack of 3 calls and calls that include at least `data` and `...` formals (`harmonizer` functions specific) up to `.GlobalEnv` or `harmonize` call.
##' 
##' @return Returns list of updated arguments specified in `harmonize_options` function
##' 
##' 
##' @md 
##' @import dotsR
##' @export 
get_harmonize_options <- function() {
    evalq({
        get_dots(harmonize_options
               , search_while_calls_have_formals = c("x", "...")
               , search_up_nframes = 5L
               , search_up_to_call = c("harmonize", "harmonizer::harmonize")
               , skip_checks_for_parent_call = FALSE)
    }, envir = parent.frame())
}
## --------<<  get_harmonize_options:1 ends here



## -------->>  [[id:org:p11ds0x069j0][inset_vector:1]]
##' Insets target vector back to input object (`data`)
##'
##' @param data an object
##' @param vector a vector to inset. Optional. Default is NULL
##' @param col vector of interest in `data` object
##' @param update Update values in `col` column. Default is FALSE. If set `append`, `name` and `name_suffix` are ignored. Also if set the default for `select_all_data_cols` will be set to TRUE.
##' @param rows Logical vector to filter records of interest. Default is NULL which means do not filter records
##' @param ommitted_rows_values If `rows` is set merge these values to the results. It should be a vector of length 1 or `nrow(data)`. If the value is NULL (default) then use original values of `col`.
##' @param append If set then put `vector` as the last instead of first vector/column. Default is FALSE.
##' @param name_suffix If `name` is not set the use this as suffix (default is "harmonized"). If the name with the same suffix already edataists in `select_data_cols` it will add counter at the end to avoid variables with the same names.
##' @param name Use this name for the first column in results (harmonized names). Default is NULL, which means that either name_if_data_atomic if data is vector or original col name will be used with `name_suffix` at the end.
##' @param name_if_data_atomic If `data` is vector use this name for original column if it is in results. Default is "data". If `data` is table the name of `col` will be used.
##' @param select_data_cols If data is table, set the columns to cbind to the result table. Default is cbind all but the original (unharmonized) column (col).
##' @param select_all_data_cols Whether to bind all columns in `data`. Defaults depends on values of `update` and `append`. If either is set then defaut values is TRUE otherwise FALSE. If set to TRUE by user the select_data_cols is ignored.
##'
##' @return returns updated `data` object
##'
##' @md
##' @import magrittr stringr data.table
inset_vector <- function(data
                       , vector
                       , col = 1
                       , update = FALSE
                       , append = FALSE
                       , rows = NULL
                       , ommitted_rows_values = NULL
                       , name = NA
                       , name_suffix = "harmonized"
                       , name_if_data_atomic = "names"
                       , select_data_cols =
                             -ifelse(is.numeric(col), col, match(col, names(data)))
                       , select_all_data_cols = append | update ) {
    ## process arguments
    check_args_col_rows()
    check_args_for_inset_vector()    
    ## inset filtered rows (this makes list if vector is list)
    vector %<>% inset(ommitted_rows_values, rows, .)
    ## inset vector to data
    if(is.atomic(data) & isFALSE(select_data_cols == 1)) {
        vector
    } else if(update) {
        data %>%
            harmonize_defactor(conv2dt = "all") %>% 
            inset2(col, value = vector) %>% 
            extract(., ,select_data_cols, with = FALSE)
    } else if(isTRUE(select_data_cols == 0)) {
        vector
    } else {
        ## set harmonized name
        data.names <- if(is.atomic(data)) name_if_data_atomic else names(data)
        name %<>%
            harmonize_eval_if_empty(
                harmonize_add_suffix(data.names[col]
                                   , name_suffix
                                   , data.names[select_data_cols])) %>%
            make.names
        ## (pre)append vector to data
        data %<>% harmonize_defactor(conv2dt = "all") # returns data.table
        vector %>%
            data.table %>%          # should make one column even if inset is list
            set_names(name) %>%
            harmonize.x.cbind(data[, select_data_cols, with = FALSE], append)
    }
}





##' Insets target vector back to input object (`data`)
##'
##' @param data an object
##' @param vector a vector to inset. Optional. Default is NULL
##' @inheritDotParams harmonize_options
##' @return returns updated `data` object
##' 
##' @md 
##' @importFrom magrittr %>%
##' @import magrittr data.table dplyr stringr
##' @export 
inset_vector_new <- function(x
                           , vector
                           , ...) {
    dots <- get_harmonize_options()
    for (v in names(dots)) {
        assign(v, dots[[v]])
    }
    ## process arguments
    check_args_col_rows()
    ## check_args_for_inset_vector()    
    ## inset filtered rows (this makes list if vector is list)
    vector %<>% inset(ommitted_rows_values, rows, .)
    ## inset vector to data
    if(is.atomic(x) && isFALSE(select_cols == 1)) {
        vector
    } else if(placement == "replace_col") {
        data %>%
            harmonize_defactor(conv2dt = "all") %>% 
            inset2(col, value = vector) %>% 
            extract(., ,select_cols, with = FALSE)
    } else if(isTRUE(select_cols == 0)) {
        vector
    } else {
        ## set harmonized name
        data.names <- if(is.atomic(data)) name_if_data_atomic else names(data)
        name %<>%
            harmonize_eval_if_empty(
                harmonize_add_suffix(data.names[col]
                                   , name_suffix
                                   , data.names[select_cols])) %>%
            make.names
        ## (pre)append vector to data
        data %<>% harmonize_defactor(conv2dt = "all") # returns data.table
        vector %>%
            data.table %>%          # should make one column even if inset is list
            set_names(name) %>%
            harmonize.x.cbind(data[, select_cols, with = FALSE], append)
    }
}

check_args_for_inset_vector <- function(env = parent.frame()) {
    evalq({
        ## - check vector
        harmonize_is_ok_type(vector
                           , allow.null = FALSE
                           , x.length = if(isTRUE(rows)) harmonize_data_length(x)
                                        else sum(rows)
                           , type = c("atomic", "list"))
        ## - check ommitted_rows_values
        if(!harmonize_is_ok_type(ommitted_rows_values
                               , x.length = c(1, harmonize_data_length(x))
                               , type = "atomic")) {
            ommitted_rows_values <- harmonize_data_get_col(x, col)
        } else if(length(ommitted_rows_values) == 1) {
            ommitted_rows_values %<>% harmonize_defactor %>% rep(harmonize_data_length(x))
        } else {
            ommitted_rows_values %<>% harmonize_defactor
        }
        ## - check select_cols
        harmonize_is_ok_type(select_all_data_cols)
        ## select_all_data_cols could be TRUE if append | update
        ## if select_all_data_cols is not set manually but select_cols is
        ## then respect select_data_cols
        if(select_all_data_cols && (!missing(select_all_data_cols) || missing(select_cols)))
            ## set select_data_cols to all
            select_data_cols <- 1:harmonize_data_width(x)
        else if(harmonize_is_ok_col(select_data_cols, x
                                  , allow.negative = TRUE
                                  , several.ok = TRUE))
            select_data_cols %<>% switch(is.numeric(.) + 1, match(., names(x)), .)
        else {
            ## set it to zero if it is null
            select_data_cols <- 0
        }
        ## - check append
        harmonize_is_ok_type(append)
        ## - check update
        harmonize_is_ok_type(update)
        if(update &&
           ((all(select_data_cols < 0) && (-col %in% select_data_cols)) ||
            (all(select_data_cols >= 0) && !(col %in% select_data_cols)))) {
            stop("'update' is set but 'col' is excluded by 'select_data_cols'")
        }
        ## - check names
        if(!update) {
            harmonize_is_ok_type(name, x.length = 1
                               , type = "character", allow.null = FALSE)
            if(name %in% names(x)[select_data_cols]) {
                stop("The harmonized column name: ", name, " is alredy exists.")
            }
            harmonize_is_ok_type(name_suffix
                               , x.length = 1
                               , type = "character"
                               , allow.na = FALSE, allow.null = FALSE)
            harmonize_is_ok_type(name_if_data_atomic
                               , x.length = 1
                               , type = "character"
                               , allow.na = FALSE, allow.null = FALSE)
        }
    }, envir = env)
}
## --------<<  inset_vector:1 ends here



## -------->>  [[id:org:ngbgs341vli0][harmonize.x.dots:1]]
##' Same as `harmonize.x` but checks and updates dots values if needed. Runs only in environment where ... (dots) ment for `harmonize.x` exists.
##' 
##' @param x Table or vector
##' @param inset.vector Vector to inset to `x`. If not provided it will return a vector.
##' @param env Do not set. This function needs its calling environment.
##' @param dots Do not set. This function needs ... (dots) values of calling environment.
##' @inheritDotParams harmonize.x
##' @return Table or vector
##' 
##' @md 
##' @import magrittr
##' @export 
harmonize.x.dots <- function(x
                           , inset.vector = NULL
                           , env = parent.frame()
                           , dots = eval(expression(list(...)), envir = env)
                           , ...) {
  ## check dots for consistensy
  formals.names <-
    formals("harmonize.x") %>%
    names %>%
    extract(!(. %in% c("x", "inset.vector")))
  harmonize_is_ok_dots(names(dots), formals.names)
  ## check ... (args) for consistensy
  args <- as.list(match.call()) %>%
    extract(-1) %>%
    extract(!(names(.) %in% c("x", "inset.vector", "dots", "env")))
  ## smart inset from ... to dots
    if(harmonize_is_ok_dots(names(args), formals.names)) {
        ## remove old args
        dots[names(dots) %in% names(args)] <- NULL
        ## get new args
        args.val <- sapply(args, eval, envir = env, simplify = FALSE)
        ## add new args to does
        dots %<>% c(args.val, .)
  }          
  ## call harmonize.x with updated dots
  do.call("harmonize.x", c(list(x = x, inset.vector = inset.vector), dots))
}


##' If no argument `arg.name` is provided in ... (dots) then use `arg.val` as new default. Otherwise use value specified in ... (dots) by user.
##' 
##' @param arg.name Name of argument in dots to update defaults
##' @param arg.val New default value
##' @param env Do not set. This function needs its calling environment.
##' @param dots Do not set. This function needs ... (dots) values of calling
##' @return Updated value for dots argument.
##' 
##' @md 
dots.default <- function(arg.name, arg.val
                       , env = parent.frame()
                       , dots = eval(expression(list(...)), envir = env)) {
  if(arg.name %in% names(dots))
    dots[[arg.name]]
  else eval(arg.val, envir = env)
}


##' Combines (with `&` funciton) two logical vectors. One is `arg.name` from dots the other is `arg.val`. Checks if they are the same length.
##' 
##' @param arg.name Name of argument in dots to combine (&) `arg.val` with
##' @param arg.val Logical vector for combining with `arg.name` value in dots
##' @param env Do not set. This function needs its calling environment.
##' @param dots Do not set. This function needs ... (dots) values of calling 
##' @return Updated value for dots argument.
##' 
##' @md 
dots.and <- function(arg.name, arg.val
                   , env = parent.frame()
                   , dots = eval(expression(list(...)), envir = env)) {
  harmonize_is_ok_type(arg.val, allow.na = FALSE, allow.null = FALSE, type = "logical")
  if(arg.name %in% names(dots)) {
    dots.logical <- dots[[arg.name]]
    ## dots.logical <- get(arg.name, envir = env)
    if(!harmonize_is_ok_type(dots.logical, length(arg.val), allow.na = FALSE)) {
      dots.logical <- TRUE
    }
    eval(arg.val, envir = env) & dots.logical
  } else eval(arg.val, envir = env)
}
## --------<<  harmonize.x.dots:1 ends here



## -------->>  [[id:org:i762gum0fqi0][harmonize.make.procedures.list:1]]
##' Makes list of procedures calls from table.
##'
##' Table should have at least two columns - messages and fuctions calls. Each function call should be a string of the following format "'function.name', arg1 = val1, arg2 = val2" (same as arguments for `do.call` function).
##' 
##' @param procedures.table Table to use
##' @param message.field name of the column with messages that will be displayed when each call is executed
##' @param function.call.field name of the column where function (harmonization procedures) calls are listed.
##' @param no.field name of the column where the number of procedure is specified. Also this field indicates if the row in the table is just a comment in which case it will be removed if `remove.comments` is set (which is set by default)
##' @param remove.comments Whether to remove comments.
##' 
##' @return List of named function calls. Names are messages.
##' 
##' @md 
##' @import magrittr data.table
harmonize.make.procedures.list <- function(procedures.table
                                         , message.field = "message"
                                         , function.call.field = "function.call"
                                         , no.field = "no"
                                         , remove.comments = TRUE
                                         , sort.by.no.field = TRUE
                                         , comments = c("#", "-", "")) {
    procedures.table %<>% harmonize_defactor
    if(remove.comments) {
        procedures.table %<>%
            extract(!(procedures.table[[no.field]] %in% comments), )
    }
    if(sort.by.no.field) {
        procedures.table %<>%
            extract(order(procedures.table[[no.field]]), )
    }
    procedures.table %>% 
        extract2(function.call.field) %>%
        paste0("list(", ., ")") %>%
        lapply(function(str) eval(parse(text = str))) %>%
        lapply(function(lst) if(length(lst) == 1) unlist(lst) else lst) %>% 
        set_names(procedures.table[[message.field]])
}
## --------<<  harmonize.make.procedures.list:1 ends here



## -------->>  [[id:org:ije1f8s0lei0][harmonize.x.split:1]]
##' Splits the object (table) in chunks by rows
##'
##' Convenient to apply some function to the table in chunks, e.g., if you want to add display of progress.
##'
##' @param x object or table
##' @param by number of rows to split by
##' @param len length of the table (nrow)
##' 
##' @return List of (sub)tables
##'
##' @export
 harmonize.x.split <- function(x, by, len) {
   split(x, rep(seq(1, len %/% by +1)
              , each = by
              , length.out = len))
 }
## --------<<  harmonize.x.split:1 ends here



## -------->>  [[id:org:dlp0f8s0lei0][harmonize.squish.spaces:1]]
#' Removes redundant whitespases
#' @param x table or vector
#'
#' @param wrap.in.spaces If set then adds leaing and ending spaces. Default is FALSE.
#'
#' @inheritDotParams harmonize.x
#'
#' @return updated table or vector
#' @import magrittr stringr
#' @export
harmonize.squish.spaces <- function(x, wrap.in.spaces = FALSE, ...) {
  harmonize.x(x, ...) %>% # get x.vector
    str_squish %>%
    {if(wrap.in.spaces) paste0(" ", ., " ") else .} %>% 
    harmonize.x(x, ., ...) # put x.vector to x
}

## #' Removes redundant whitespases
## #' @param x table or vector
## #'
## #' @param trim Whether to trim the beging (i.e., "left"), ending (i.e., "right") or "both" whitespaces.
## #' @inheritDotParams harmonize.x
## #'
## #' @return updated table or vector
## #' @import magrittr stringr
## #' @export
## harmonize.clean.spaces <- function(x
##                                  , trim = "both"
##                                  , ...) {
##   harmonize.x(x, ...) %>% # get x.vector
##     str_replace_all("\\s+", " ") %>%
##     {if(is.null(trim)) .
##      else stri_trim(., side = trim)} %>%
##     harmonize.x(x, ., ...) # put x.vector to x
## }
## --------<<  harmonize.squish.spaces:1 ends here



## -------->>  [[id:org:xys0f8s0lei0][harmonize.toupper:1]]
##' Uppercases vector of interest in the object (table)
##' 
##' @param data data
##' 
##' @inheritDotParams inset_vector
##'
##' @import magrittr
##' 
##' @return updated data (as data.table)
##' @export
harmonize.toupper <- function(data, ...) {
  get_vector(data, ...) %>% 
    toupper %>% 
    inset_vector(data, ., ...)
}
## --------<<  harmonize.toupper:1 ends here



## -------->>  [[id:org:9ew0f8s0lei0][harmonize.remove.brackets:1]]
##' Removes brackets and content in brackets
##' @param x object (table)
##' @inheritDotParams harmonize.x
##' @return updated object
##' 
##' @import stringr magrittr
##' @export
harmonize.remove.brackets  <- function(x, ...) {
  harmonize.x(x, ...) %>% 
    str_replace_all("<[^<>]*>|\\([^()]*\\)|\\{[^{}]*\\}|\\[[^\\[\\]]*\\]", "") %>%
    harmonize.x(x, ., ...)
}
## --------<<  harmonize.remove.brackets:1 ends here



## -------->>  [[id:org:4vz0f8s0lei0][harmonize.remove.quotes:1]]
##' Removes double quotes (deprecated)
##' 
##' (This is a separate procedure because read.csv can not get this substitution in old version of harmonizer)
##'
##' @param x an object
##' @inheritDotParams harmonize.x
##' @return updated object
##' 
##' @import stringr magrittr
harmonize.remove.quotes <- function(x, ...) {
  harmonize.x(x, ...) %>% 
    stri_replace_all_fixed("\"", "") %>% 
    harmonize.x(x, ., ...)
}
## --------<<  harmonize.remove.quotes:1 ends here



## -------->>  [[id:org:3ya1f8s0lei0][harmonize.unlist.column:1]]
##' If column in the `x` table is list unlist it if possible
##' @param x object
##' @return updated object
##' @export
harmonize.unlist.column <- function(x) {
  if(is.atomic(x)) x
  else if(is.list(x)) {
    len <- sapply(x, length)
    if(all(len == 1))
      unlist(x)
    else if(all(len %in% 0:1))
      unlist(inset(x, len == 0, NA))
    else x
  } else x
}
## --------<<  harmonize.unlist.column:1 ends here



## -------->>  [[id:org:4tffib50bci0][harmonize.dehtmlize:1]]
#' Converts HTML characters to UTF-8 (this one is 1/3 faster than htmlParse but it is still very slow)
## from - http://stackoverflow.com/questions/5060076
#' @param x object (table)
#' @param as.single.string If set then collapse characters in the main column of the `x` (i.e., `x.col`) as to a single string. It will increase performance (at least for relatively short tables). Default is FALSE
#' @param as.single.string.sep delimiter for collapsed strings to uncollapse it later. Default is "#_|".
#' @param read.xml If set the it will parse XML. Default is FALSE which means it parses HTML
#' @inheritDotParams harmonize.x
#' @return updated object
#'
#' @import xml2 magrittr
#' @export
harmonize.dehtmlize <- function(x
                              , as.single.string = FALSE
                              , as.single.string.sep = "#_|"
                              , read.xml = FALSE
                              , ...) {
  x.vector <- harmonize.x(x, ...)
  if(as.single.string) {
    x.vector %>%
      paste0(collapse = as.single.string.sep) %>%
      paste0paste0("<x>", ., "</x>") %>% 
      {if(read.xml) read.xml(.)
       else read_html(.)} %>%
      xml_text %>% 
      strsplit(as.single.string.sep, fixed = TRUE)[[1]]
  } else {
    sapply(x.vector, function(str) {
      paste0("<x>", str, "</x>") %>%
        {if(read.xml) read.xml(.)
         else read_html(.)} %>%
        xml_text
    })    
  } %>% 
    harmonize.x(x, ., ...) %>%
    return()
}
## --------<<  harmonize.dehtmlize:1 ends here



## -------->>  [[id:org:e2bfib50bci0][harmonize.detect.enc:1]]
#' Detects string encoding
#' @param x object
#' @param codes.append basically `harmonized.append` parameter passed to `harmonize.x` but with new defaults. Default is TRUE.
#' @param codes.suffix basically `harmonized.suffix` parameter passed to `harmonize.x` but with new defaults. Default is "encoding"
#' @param return.codes.only If set it overwrites `return.x.cols` and `x.harmonized.col.update` parameters passed to `harmonize.x`. Default is FALSE.
#' @inheritDotParams harmonize.x
#' @return updated object
#'
#' @import stringi magrittr
#' @export
harmonize.detect.enc <- function(x
                               , codes.append = TRUE
                               , codes.suffix = "encoding"
                               , ...) {
  available.enc.list <- iconvlist()
  x.vector <- harmonize.x.dots(x
                             , harmonized.suffix = codes.suffix
                             , harmonized.append = codes.append)
  stri_enc_detect(x.vector) %>%
    lapply(function(enc) {
      enc %<>% extract2("Encoding")
      first.ok.enc <- (enc %in% available.enc.list) %>% which %>% extract(1)
      if(length(first.ok.enc) == 0) ""
      else enc[[first.ok.enc]]
    }) %>%
    unlist %>%
    harmonize.x.dots(x, .
                   , harmonized.suffix = codes.suffix
                   , harmonized.append = codes.append) %>% 
    return()
}
## --------<<  harmonize.detect.enc:1 ends here



## -------->>  [[id:org:mzn0tpb0wei0][harmonize.toascii:1]]
#' Translates non-ascii symbols to its ascii equivalent
#' 
#' @param str String to translate
#' @param detect.encoding Detect encoding of individual elements
#' @inheritDotParams harmonize.x
#' 
#' @import stringi stringr magrittr
#' 
#' @export
harmonize.toascii <- function(x
                            , detect.encoding = FALSE
                            , ...) {
  str <- harmonize.x(x, ...)
  utf <- harmonizer.patterns.ascii$utf %>% paste(collapse = "")
  ascii <- harmonizer.patterns.ascii$ascii %>% paste(collapse = "")
  {if(detect.encoding)  # detect encoding of individual elements
     mapply(function(name, enc)
       iconv(name
           , from = enc
           , to = "UTF-8"
           , sub = "") %>%
       {chartr(utf, ascii, .)}
     , str
     , harmonize.detect.enc(str, return.x.cols = NULL)
     , SIMPLIFY = FALSE, USE.NAMES = FALSE) %>%
       unlist %>% 
       iconv(to = "ASCII", sub = "")
   else
     enc2utf8(str) %>% 
       {chartr(utf, ascii, .)} %>% 
       iconv(to = "ASCII", sub = "")} %>%
    harmonize.x(x, ., ...)
}
## --------<<  harmonize.toascii:1 ends here



## -------->>  [[id:org:g18cg5z0nmi0][harmonize.match.arg:1]]
##' Matches the argument vector to (default) choices and ensures the correct length
##' @param arg An argument vector to check if it is matches the values
##' @param arg.length Desired length of the `arg` to check against or to ensure
##' @param arg.length.check Whether to check `arg` length
##' @param ensure.length Whether to repeat `arg` `length` times if `arg` is length of 1
##' @param choices Vector of values to match `arg`
##' @param arg.call Saves `arg` call. Do not touch!
##' @param env Saves environment where the function was called from. Do not touch!
##' @param length Check if the `arg` is this lenght. If `arg` is length of 1 and `ensure.length` is set to TRUE (default) then it will repeat `arg` `length` times and return
##' @return Argument vector
##' 
##' @md 
##' @import magrittr data.table dplyr stringr
##' @export 
harmonize.match.arg <- function(arg
                              , arg.length = 1
                              , arg.length.check = TRUE
                              , ensure.length = TRUE
                              , choices = NULL
                              , arg.call = substitute(arg)
                              , env = parent.frame()) {
  ## check arguments
  harmonize_is_ok_type(arg, type = "atomic")
  harmonize_is_ok_type(arg.length, type = "numeric", x.length = 1)
  harmonize_is_ok_type(choices, type = "atomic")
  harmonize_is_ok_type(arg.length.check, type = "logical")
  harmonize_is_ok_type(ensure.length, type = "logical")
  ## fools protection
  ## if(!missing(env) | !missing(arg.call))
  ##   stop("Arguments 'arg.call' and 'env' should not be set")
  ## if choices are missing use defaults
  if(missing(choices)) {
    choices <- eval(evalq(formals(), envir = env)[[deparse(arg.call)]])
  }
  ## if arg is missing use first element
  if(missing(arg.call) && # allow to provide alternative defaults
     deparse(arg) != arg.call && # check if an argument is not the value
     eval(call("missing", arg.call), envir = env)) {
    arg <- choices[[1]]
  }
  ## check if arg matches choices and length
  arg %<>% harmonize_defactor_vector
  if(all(arg %in% choices)) {
    if(arg.length.check && ensure.length && length(arg) == 1)
      return(rep(arg, arg.length))
    else if(arg.length.check && length(arg) != arg.length)
      stop("'", arg.name, "' should be of length ", arg.length)
    else
      return(arg)
  } else {
    stop("Argument does not match choices/defauls '", deparse(choices), "'!")
  }
}
## --------<<  harmonize.match.arg:1 ends here



## -------->>  [[id:org:xcpfib50bci0][harmonize.replace:1]]
#' A wrapper for string replacement and cbinding some columns.
#'
#' Optionally matches only at the beginning or at the end of the string.
#' 
#' @param x Vector or table to harmonize.
#' @param patterns Accepts both vector or table. If patterns it is table can also include replacements column.
#' @param patterns.col If patterns is not a vector which column to use. Default is 1.
#' @param patterns.mode Mode of matching. Could be one of c("all", "first", "last"). The default is "all" (it is 2x faster than "first" and "last" because of handy stri_replace_all_* functions). Also possible to pass a vector (same length as patterns)
#' @param patterns.mode.col  Column in patterns table with the mode of matching
#' @param patterns.type Kind of pattern. Default is "fixed" (calling code{\link[stringi]{stri_replace_all_fixed}}). Other options are "begins", "ends" - which means that it should only match fixed pattern at the beginning of the string or at the and. Another possible value is "regex" (calling code{\link[stringi]{stri_replace_all_regex}})
#' @param patterns.type.col Column with the type of pattern in case when patterns should have different types
#' @param patterns.replacements.col If patterns is not a vector and includes replacements which column to use for replacements. Default is 2.
#' @param replacements If patterns does not have column with replacements provide it here.
#' @inheritDotParams harmonize.x
#'
#' @return If nothing was indicated to cbind to results then it returns harmonized vector. If something is needs to be cbind then it returns data.table
#' @import stringi stringr magrittr
#' 
#' @export
harmonize.replace <- function(x
                            , patterns
                            , patterns.col = 1
                            , patterns.mode = c("all", "first", "last")
                            , patterns.mode.col = NULL
                            , patterns.type = c("fixed"
                                              , "begins"
                                              , "begins.trimmed"
                                              , "ends"
                                              , "ends.trimmed"
                                              , "regex"
                                              , "exact"
                                              , "exact.trimmed")
                            , patterns.type.col = NULL
                            , patterns.replacements.col = 2
                            , replacements = NULL
                            , ...) {
    ## check arguments and get vectors
    x.vector <- harmonize.x(x, ...)
    patterns.vector <- harmonize.x(patterns, x.col = patterns.col)
    types.vector <- harmonize.replace..get.types.vector()
    modes.vector <- harmonize.replace..get.modes.vector()
    replacements.vector <- harmonize.replace..get.replacements.vector()
    ## do replace and return
    harmonize.replace..do() %>% harmonize.x(x, ., ...)
}

harmonize.replace..get.types.vector <- function(env = parent.frame()) {
  evalq({
    ## patterns.vector should be ready
    if(missing(patterns.type.col)) {
      harmonize.match.arg(patterns.type
                        , arg.length = length(patterns.vector))
    } else {
      harmonize.match.arg(arg = harmonize.x(patterns, x.col = patterns.type.col)
                        , arg.length = length(patterns.vector)
                        , arg.call = quote(patterns.type))
    }
  }, envir = env)
}

harmonize.replace..get.modes.vector <- function(env = parent.frame()) {
    evalq({
    ## patterns.vector should be ready
    if(missing(patterns.mode.col)) {
      harmonize.match.arg(patterns.mode, arg.length = length(patterns.vector))
    } else {
      harmonize.match.arg(arg = harmonize.x(patterns, x.col = patterns.mode.col)
                        , arg.length = length(patterns.vector)
                        , arg.call = quote(patterns.mode))
    }
  }, envir = env)
}

harmonize.replace..get.replacements.vector <- function(env = parent.frame()) {
evalq({
    ## patterns.vector should be ready
    if(missing(replacements) && !is.atomic(patterns)) {
      harmonize.x(patterns, x.col = patterns.replacements.col)
    } else if(harmonize_is_ok_type(replacements
                                 , x.length = c(1, length(patterns.vector))
                                 , type = "atomic")) {
        harmonize_defactor_vector(replacements) %>%
            {if(length(.) == 1) rep(., length(patterns.vector)) else .}
    } else {
        ## replace with nothig by default
        rep("", length(patterns.vector))
    }
  }, envir = env)
}

## a wrapple for stri_replace to use in Reduce
stri_replace.do <- function(str, arg.list) {
  do.call(stri_replace, c(list(str), arg.list))
}

harmonize.replace..do <- function(env = parent.frame()) {
  evalq({
    ## make patterns.vector excaped according to types.vector
    patterns.vector %<>% harmonize_escape_types(types.vector)
    ## conditions are organized from fastest to slowest replace procedures
    if(all(types.vector == "exact") || all(types.vector == "exact.trimmed")) {
      x.vector %>% 
        {if(all(types.vector == "exact.trimmed")) str_trim(.) else .} %>% 
        match(patterns.vector) %>% 
        extract(replacements.vector, .) %>% 
        inset(x.vector, !is.na(.), .)
    } else if(all(modes.vector == "all")) {
      if(all(types.vector == "fixed")) {
        stri_replace_all_fixed(x.vector
                             , patterns.vector
                             , replacements.vector
                             , vectorize_all = FALSE)
      } else {
        stri_replace_all_regex(x.vector
                             , patterns.vector
                             , replacements.vector
                             , vectorize_all = FALSE)
      }
    } else if(all(types.vector == "fixed")) {
      Map(list
        , fixed = patterns.vector
        , replacement = replacements.vector
        , mode = modes.vector) %>%
        Reduce(stri_replace.do, ., init = x.vector) # same as for loop
    } else {
      Map(list
        , regex = patterns.vector
        , replacement = replacements.vector
        , mode = modes.vector) %>%
        Reduce(stri_replace.do, ., init = x.vector) # same as for loop
    }
  }, envir = env)
}
## --------<<  harmonize.replace:1 ends here



## -------->>  [[id:org:bb21tpb0wei0][harmonize.detect:1]]
#' This function is basically meant for coding names based on certain pattern
#'
#' Optionally matches only at the beginning or at the end of the string.
#' 
#' @param x Vector or table to detect in.
#' @param patterns Accepts both vector or table. If patterns it is table can also include replacements column.
#' @param patterns.col If patterns is not a vector specifies which column to use. Default is 1.
#' @param patterns.as.codes If set then use patterns as codes. In that case codes are ignored and there is no need to provide them. Default is FALSE.
#' @param patterns.codes.col If patterns is table which column to use as codes column. Default is 2.
#' @param patterns.type Specifies kind(s) of patterns. Default is "fixed" (calling code{\link[stringi]{stri_replace_all_fixed}}). Other options are "begins", "ends" - which means that it should only match fixed pattern at the beginning of the string or at the and. Another possible value is "regex" (calling code{\link[stringi]{stri_replace_all_regex}}). Another possible options are "exact" and "trim.exact" which matches a pattern string exactly or its trimmed version (i.e., white spaces are ignored at the beginning and at the end of the pattern)
#' @param patterns.type.col Column in patterns table where you can specify types of patterns for each pattern. If set then `patterns.type` is ignored. Default is NULL
#' @param codes If provided use it as codes. Should be the same length as patterns. Default is NULL.
#' @param codes.name If provided use it as a name for codes column in results. It also will work as `x.codes.col` if `x.codes.merge` or `x.codes.update.empyty` are set. In case if `x.codes.merge` or `x.codes.update.empyty` are set and names(`x`) does not have column name `codes.name` it will reset both `x.codes.merge` and `x.codes.update.empyty` to FALSE and will (pre)append a new column `codes.name`.
#' @param codes.suffix If `codes.name` is not provided use this suffix to append to `x.col` name or `x.atomic.name` if x is vector (see harmonize.x function).
#' @param codes.omitted.val If `x.rows` is set. Use this value to fill the omitted rows. Default is vector of NAs of `x` length. When we update existing codes column (i.e., `x.codes.col` is set and valid) and the value is not set explicitly it resets to NULL which means that we use initial values for omitted rows.
#' @param codes.prepend Whether to prepend codes column to the `x`. Default is FALSE. If set it will prepend codes column to `x` instead of append.
#' @param x.codes.col If `x` is table, which column to use for making/merging/adding newly detected codes. Default is NULL. When it is not set explicitly and `x.codes.merge` or `x.codes.update.empty` are set it will first try to use `codes.name` if it is provided or if not will set the last column of `x` as `x.codes.col`.
#' @param x.codes.update.empty If set then detect and add new codes only for records (rows) that were not yet coded (i.e., related codes are either "", NA or length == 0).
#' @param x.codes.merge If set then merge (append or prepend) new codes to existing ones. It might turn the codes column to list of vectors.
#' @param x.codes.merge.prepend If set then it will prepend new codes if not then it will append (default). It is ignored if `x.codes.merge` is not set.
#' @param return.only.codes If set then just return codes vector. Default is FALSE. Basically when it is set it changes `return.x.cols` to 0.
#' @param return.only.first.detected.code If TRUE then return only codes for the first detected pattern. If FALSE return list of vectors of all matched codes. Default is TRUE.
#' 
#' @inheritDotParams harmonize.x
#' 
#' @return The updated `x` table with codes column or just codes if `return.only.codes` is set.
#'
#' @import stringi stringr magrittr
#' 
#' @export
harmonize.detect <- function(x
                           , patterns
                           , patterns.col = 1
                           , patterns.as.codes = FALSE
                           , patterns.codes.col = 2
                           , patterns.type = c("fixed"
                                              , "begins"
                                              , "begins.trimmed"
                                              , "ends"
                                              , "ends.trimmed"
                                              , "regex"
                                              , "exact"
                                              , "exact.trimmed")
                           , patterns.type.col = NULL
                           , codes = NULL
                           , codes.name = NA
                           , codes.suffix = "coded"
                           , codes.omitted.val = NA
                           , codes.prepend = FALSE
                           , x.codes.col = NULL
                           , x.codes.update.empty = FALSE
                           , x.codes.merge = FALSE
                           , x.codes.merge.prepend = FALSE
                           , return.only.codes = FALSE
                           , return.only.first.detected.code = FALSE
                           , ...) {
    ## set x.rows.codes.update for dots.and("x.rows")
    x.rows.codes.update <- rep(TRUE, harmonize_data_length(x)) # by defaults updates all codes
    harmonize.detect..check.args()                           # also sets x.rows.codes.update
    ## --------------------------------------------------------------------------------
    x.vector <- harmonize.x(x, x.rows = dots.and("x.rows", x.rows.codes.update))
    patterns.type.vector <- harmonize.detect..get.patterns.type.vector()
    patterns.vector <-  harmonize.detect..get.patterns.vector()
    codes.vector <- harmonize.detect..get.codes.vector()
    x.codes.vector <- harmonize.detect..get.x.codes.vector()
    ## --------------------------------------------------------------------------------
    ## if there is something to detect in
    x.inset.vector <- if(length(x.vector) !=0) {
                          harmonize.detect..do.vector()
                      } else {
                          ## if x.vector is empty then skip detection and inset empty
                          character(0)
                      }
    ## inset codes
    harmonize.x.dots(x, x.inset.vector
                   , x.rows = dots.and("x.rows", x.rows.codes.update)
                   , x.col = x.codes.col
                   , x.col.update = if(return.only.codes) FALSE
                                    else dots.default("x.col.update"
                                                    , x.codes.update.empty | x.codes.merge)
                   , inset.omitted.val = codes.omitted.val
                   , inset.name = codes.name
                   , inset.suffix = codes.suffix
                   , inset.append = dots.default("inset.append", !codes.prepend)
                   , return.x.cols = if(return.only.codes) NULL
                                     else dots.default("return.x.cols"
                                                     , 1:harmonize_data_width(x)))
}

harmonize.detect..check.args <- function(env = parent.frame()) {
    evalq({
        ## -- patterns is check by harmonize.x
        ## -- patterns.col is check by harmonize.x
        ## -- check patterns.type by ...
        ## -- check patterns.type.col - by harmonize.detect..get.patterns.type.vector
        ## - check patterns.as.codes
        harmonize_is_ok_type(patterns.as.codes)
        ## -- check patterns.codes.col - by harmonize.detect..get.codes.vector
        ## -- check codes - by harmonize.detect..get.codes.vector
        ## -- check codes.name - also by harmonize.x
        ## -- check codes.suffix - by harmonize.x
        ## -- check codes.omitted.val - by harmonize.x
        ## -- check codes.prepend - by harmonize.x
        ## - check x.codes.merge
        harmonize_is_ok_type(x.codes.merge)
        ## - check x.codes.update.empty
        harmonize_is_ok_type(x.codes.update.empty)
        ## - check x.codes.col (should not be the same as x.col)
        if(harmonize_is_ok_col(x.codes.col, x
                             , ban.values = dots.default("x.col", 1))) {
            x.codes.col %<>% switch(is.numeric(.) + 1, match(., names(x)), .)
            ## use x.codes.col as codes.omitted.val if it is not set
            if(missing(codes.omitted.val)) codes.omitted.val <- NULL
        } else if(x.codes.merge | x.codes.update.empty) {
            ## set x.codes.col as last one
            x.codes.col <- harmonize_data_width(x)
            ## check codes.names just in case
            harmonize_is_ok_type(codes.name, x.length = 1
                               , type = "character"
                               , allow.null = FALSE)
            if(codes.name %in% names(x)) {
                ## set as x.codes.col as codes.name
                x.codes.col <- match(codes.name, names(x))
                if(missing(codes.omitted.val)) codes.omitted.val <- NULL
            } else if(!is.na(codes.name)) {
                ## if codes.name is provided and it is a new name then append codes
                x.codes.merge <- FALSE
                x.codes.update.empty <- FALSE
            } else {
                ## use x.codes.col as codes.omitted.val if it is not set
                if(missing(codes.omitted.val)) codes.omitted.val <- NULL
            }
        } else {
            ## set x.codes.col as last one anyway
            x.codes.col <- harmonize_data_width(x)
        }
        ## set x.rows.codes.update for dots.and("x.rows")
        if(x.codes.update.empty) {
            x.rows.codes.update <- harmonize_is_data_empty(x[[x.codes.col]])
            x.codes.merge <- FALSE # nothing to merge with if codes are empty
        }
        ## - check return.only.codes
        harmonize_is_ok_type(return.only.codes)
        ## - check return.only.first.detected.code
        harmonize_is_ok_type(return.only.first.detected.code)
    }, envir = env)
}

harmonize.detect..get.patterns.type.vector <- function(env = parent.frame()) {
  evalq({
        if(harmonize_is_ok_col(patterns.type.col, patterns)) {
            harmonize.x(patterns, x.col = patterns.type.col)
        }
        else {
            harmonize.match.arg(patterns.type)
        }
    }, envir = env)
}

harmonize.detect..get.patterns.vector <- function(env = parent.frame()) {
  evalq({
        harmonize.x(patterns, x.col = patterns.col) %>% 
            harmonize_escape_types(patterns.type.vector, all.regex = FALSE)
    }, envir = env)
}

harmonize.detect..get.codes.vector <- function(env = parent.frame()) {
    evalq({
        if(patterns.as.codes) patterns.vector
        else if(harmonize_is_ok_type(codes
                                   , x.length = c(1, harmonize_data_length(patterns))
                                   , type = "atomic")) {
            if(length(codes) == 1)    
                rep(harmonize_defactor(codes), harmonize_data_length(patterns))
            else harmonize_defactor(codes)
        }
        else if(harmonize_is_ok_col(patterns.codes.col, patterns))
            harmonize.x(patterns, x.col = patterns.codes.col)
        else stop("No codes provided.")
    }, envir = env)
}

harmonize.detect..get.x.codes.vector <- function(env = parent.frame()) {
    evalq({
        if(!is.null(x.codes.col))
            harmonize.x(x
                      , x.col = x.codes.col
                      , x.rows = dots.and("x.rows", x.rows.codes.update))
        else NULL
    }, envir = env)
}


## given x, patterns, types, codes vectors do detection
harmonize.detect..do.vector <- function(env = parent.frame()) {
    evalq({
        mapply(function(pattern, pattern.type, code) {
            x.vector %>% 
                {if(pattern.type == "fixed")
                     stri_detect_fixed(., pattern)
                 else if(pattern.type == "exact")
                     . == pattern
                 else if(pattern.type == "exact.trimmed")
                     str_trim(.) == pattern
                 else stri_detect_regex(., pattern)} %>% 
                ifelse(code, NA)
        }
      , patterns.vector
      , patterns.type.vector
      , codes.vector
      , SIMPLIFY = FALSE
      , USE.NAMES = FALSE) %>%
          ## transpose list of vectors
          {do.call(Map, c(list(c), .))} %>% 
          ## remove empty codes
          lapply(harmonize_omit_empty) %>%
          ## check if only first detected code is needed
          {if(return.only.first.detected.code) lapply(.,extract, 1) else .} %>% 
          ## check if we need to merge
          {if(x.codes.merge & !x.codes.merge.prepend)
               Map(c, x.codes.vector, .)
           else if(x.codes.merge & x.codes.merge.prepend)
               Map(c, ., x.codes.vector)
           else .} %>% 
          ## remove empty codes
          lapply(harmonize_omit_empty) %>%
          harmonize.unlist.column
    }, envir = env)
}
## --------<<  harmonize.detect:1 ends here



## -------->>  [[id:org:ifb5ac70uai0][harmonize:1]]
##' Harmonizes organizational names. Takes either vector or column in the table.
##' 
##' @param x object (table)
##' @param procedures Named list of procedures (closures) to apply to x. If we need to pass arguments to some of the procedures it can be done by specifying sub-list where the first element is procedure and the rest its arguments. Names of the list elements are used for progress messages. Procedures can also be passed as data.frame in which case it will be converted to list of procedures with `harmonize.make.procedures.list` (see its help for the correct format of data.frame with procedures). Default is `harmonizer.default.procedures.table`
##' @param progress Show the progress? Default is TRUE
##' @param progress.min The minimum number of rows the x should have for automatic progress estimation. If x has less rows no progress will be shown. Default is 10^5
##' @param progress.by If set it will divide the x into chunk of this amount of rows. Default is NA.
##' @param progress.percent Number of percents that represent one step in progress. Value should be between 0.1 and 50. Default is 1 which means it will try to chunk the x into 100 pieces.
##' @param progress.message.use.names Should we use names from `procedures` list to report progress. Default is TRUE.
##' @param quite Suppress all messages. Default is FALSE.
##' @inheritDotParams harmonize.x
##' 
##' @return
##'
##' @import stringi stringr magrittr
##' @export
harmonize <- function(x
                    , procedures = harmonizer.default.procedures.table
                    , progress = TRUE
                    , progress.min = 10^5
                    , progress.by = NA
                    , progress.percent = 1
                    , progress.message.use.names = TRUE
                    , quite = FALSE
                    , ...) {
  if(is.data.frame(procedures)) {
        procedures %<>% harmonize.make.procedures.list
  }
  ## make format of the massages for procedures
  message.delimiter <- paste(c("\n", rep("-", 65), "\n"), collapse = "")
  message.init <- paste0("\nApplying harmonization procedures:", message.delimiter)
  message.done  <- "\b\b\b\bDONE"
  progress.format <- "\b\b\b\b%3.0f%%"
  message.format <- "* %-60.60s...."
  message.fin <- paste0(message.delimiter, "Harmonization is done!\n")
  ## check progress.percent
  if(progress.percent < 0.1 | progress.percent > 50)
    stop("Please, set progress.percent between 0.1 and 50")
  ## ensure that x is either vector or data.table
  x %<>% {
    if(is.atomic(.)) .
    else if(is.data.table(.)) .
    else if(is_matrix(.)) as.data.table(.)
    else if(is_tible(.)) as.data.table(.)
    else if(is.data.frame(.)) as.data.table(.)
    else if(is.list(.)) stop("x is list. Please, provide either vector or table")
  }
  ## Set progress.by
  progress.by <- if(!progress | quite) NA
                 else {
                   ## calculate the length of the x
                   x.length <- x %>% {if(is.atomic(.)) length(.) else nrow(.)}
                   if(x.length < progress.min) NA
                   else if(!is.na(progress.by)) {
                     ## if progress.by is set check if it is
                     ## at least twice less than x.length
                     ## and more that 1/1000 of x.length
                     if(progress.by > x.length/1000 &
                        progress.by*2 < x.length) progress.by
                     else NA
                   } else round(x.length/(100/progress.percent))
                 }
  ## Apply Procedures
  if(!quite) message(message.init)
  for(p in 1:length(procedures)) {
    ## get procedure function
    procedure.fun <- procedures[[p]] %>% extract2(1)
    ## get procedure arguments
    procedure.args <- procedures[[p]] %>%
      ## remove progress arg if it is there
      extract(-c(1, which(names(.) == "progress")))
    ## get procedure names
    procedure.name <- names(procedures)[p] %>%
        {if(harmonize_is_data_empty(.) | !progress.message.use.names)
             procedure.fun
         else .}
    ## Anounce Procedure Name
    if(!quite) packageStartupMessage(sprintf(message.format, procedure.name)
                                   , appendLF = FALSE)
    ## Check if we need report progress:
    ## progress is set & progress = FALSE is absent in the arguments
    if(!is.na(progress.by) &
       !isFALSE(procedures[[p]]["progress"][[TRUE]])) {
      ## check if we need to split..
      if(!isTRUE(class(x) == "list")) {
        x %<>% harmonize.x.split(progress.by, x.length)
      }
      ## set progress counter
      i <- 0; env <- environment()
      ## Apply procedure to list!
      x %<>% lapply(function(x.by) {
        ## apply procedure fun with args
        x.by %<>%
          list %>%
          c(procedure.args) %>%
          do.call(procedure.fun, .)
        ## Increment progress counter
        assign("i", i + 100 * progress.by / x.length, envir = env)
        ## Anounce progress
        packageStartupMessage(sprintf(progress.format, i)
                            , appendLF = FALSE)
        return(x.by)
      })
    } else {
      ## check if we need to rbindlist..
      if(isTRUE(class(x) == "list")) {
        if(is.atomic(x[[1]])) x %<>% unlist(use.names = FALSE)
        else x %<>% rbindlist
      }
      ## Apply procedure fun with args!
      x %<>% 
        list %>%
        c(procedure.args) %>%
        do.call(procedure.fun, .)
    }
    ## Anounce DONE
    if(!quite) packageStartupMessage(message.done)
  }
  if(!quite) message(message.fin)
  ## Return X
  if(isTRUE(class(x) == "list")) {
    if(is.atomic(x[[1]])) x %>% unlist(use.names = FALSE)
    else x %>% rbindlist
  } else x
}
## --------<<  harmonize:1 ends here


