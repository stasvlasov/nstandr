## -------->>  [[file:../harmonizer.src.org::*Package documentation][Package documentation:1]]
#' @details
#' Harmonizer package standardizes (harmonizes) organizational names
#'     mainly using procedures described in Thoma et al. (2010) and
#'     Magerman, Looy, Bart, & Song (2006) but not only.  This is work
#'     in progress. Please, file an issues or suggestion if you have
#'     any.  The main function is [harmonize()].
#' @keywords internal
"_PACKAGE"
## --------<<  Package documentation:1 ends here



## -------->>  [[file:../harmonizer.src.org::*harmonize_x_split][harmonize_x_split:1]]
##' Splits the object (table) in chunks by rows
##'
##' Convenient to apply some function to the table in chunks, e.g., if you want to add display of progress.
##'
##' @param x object or table
##' @param by number of rows to split by
##' @param len length of the table (nrow). If it is NULL then use x_length(x)
##' 
##' @return List of (sub)tables
harmonize_x_split <- function(x, by, len = NULL) {
    if(is.null(len)) len <- x_length(x)
    split(x, rep(seq(1, len %/% by +1)
               , each = by
               , length.out = len))
}
## --------<<  harmonize_x_split:1 ends here



## -------->>  [[file:../harmonizer.src.org::*harmonize_toupper][harmonize_toupper:1]]
##' @eval attr(harmonize_toupper, "description")
##' 
##' @param x data
##' 
##' @inheritDotParams harmonize_options
##'
##' @return updated data (as data.table)
##' @export
harmonize_toupper <- function(x, ...) {
    get_target(x) |>
        toupper() |>
        inset_target(x)
}

attr(harmonize_toupper, "description") <- 
"Uppercases vector of interest in the object (table)"
## --------<<  harmonize_toupper:1 ends here



## -------->>  [[file:../harmonizer.src.org::*harmonize.remove.brackets][harmonize.remove.brackets:1]]
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



## -------->>  [[file:../harmonizer.src.org::*harmonize.remove.quotes][harmonize.remove.quotes:1]]
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



## -------->>  [[file:../harmonizer.src.org::*harmonize.unlist.column][harmonize.unlist.column:1]]
##' If column in the `x` table is list unlist it if possible
##' @param x object
##' @param replace_zero_length_with Default is replace NULLs with NA_character_ because vector of just NA is a logical class
##' @return updated object
##' @export
unlist_if_possible <- function(x, replace_zero_length_with = NA_character_) {
    if(is.list(x)) {
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
## --------<<  harmonize.unlist.column:1 ends here



## -------->>  [[file:../harmonizer.src.org::*harmonize.dehtmlize][harmonize.dehtmlize:1]]
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



## -------->>  [[file:../harmonizer.src.org::*harmonize.detect.enc][harmonize.detect.enc:1]]
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



## -------->>  [[file:../harmonizer.src.org::*harmonize.toascii][harmonize.toascii:1]]
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



## -------->>  [[file:../harmonizer.src.org::*harmonize.match.arg][harmonize.match.arg:1]]
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
##' @import magrittr data.table stringr
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
  arg %<>% defactor_vector
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



## -------->>  [[file:../harmonizer.src.org::*harmonize.empty][harmonize.empty:1]]
##' Checks if all elements in vercor(s) are either "", NA, NULL or have zero length
##' @param x input data to check each vector
##' @param return_as_true_if_x_zero_length how to interpret zero lenth input. If TRUE then it returns TRUE. Otherwise NULL.
##' @return logical vector of the same length
harmonize_is_data_empty <- function(x
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
harmonize_omit_empty <- function(x) {
    if(length(x) == 0) return(x)
    x[!sapply(harmonize_is_data_empty(x), isTRUE)]
}



## eval things if x empty otherwise return x
harmonize_eval_if_empty <- function(x, ..., env = parent.frame()) {
  if(harmonize_is_data_empty(x))
    eval(..., envir = env)
  else x
}
## --------<<  harmonize.empty:1 ends here



## -------->>  [[file:../harmonizer.src.org::*harmonize.add.suffix][harmonize.add.suffix:1]]
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
    paste0("(?<=", escape_regex(name.with.suffix), "\\.)", "\\d+$")
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
    paste0("(?<=", escape_regex(name.with.suffix), "\\.)", "\\d+$")
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



## -------->>  [[file:../harmonizer.src.org::*harmonize.defactor][harmonize.defactor:1]]
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
##' @param ... 
##' @inheritDotParams defactor_vector
##' @return object of the same type without factors
##'  
##' @import data.table
##' 
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
## --------<<  harmonize.defactor:1 ends here



## -------->>  [[file:../harmonizer.src.org::*harmonize.x][harmonize.x:1]]
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
    harmonize_target_get(x, col = x.col, rows = x.rows)
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
            inset.omitted.val <- get_vector(x, x.col)
        } else if(length(inset.omitted.val) == 1) {
            inset.omitted.val %<>% defactor %>% rep(harmonize_data_length(x))
        } else {
            inset.omitted.val %<>% defactor
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
    get_vector(data, col)[rows]
}



## get_vector <- function(x, col, col_check = TRUE) {
##     if (is.atomic(x)) {
##         harmonize_defactor(x)
##     } else {
##         if(col_check) {
##             check_col(col, x, report_for_call = sys.call(which = -2))
##         }
##         harmonize_defactor(x[[col]])
##     }
## }

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
              defactor(conv2dt = "all") %>% 
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
          x %<>% defactor(conv2dt = "all") # returns data.table
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


## functions that only runs within get_vector and inset_vector
## --------------------------------------------------------------------------------

## Tests Arguments
check_args_col_rows <- function() {
    .Deprecated("check_col")
    .Deprecated("check_rows")
    evalq({
        ## - check col
        if(harmonize_is_ok_col(col, x, required = TRUE)) {
            col %<>% ifelse(is.numeric(.), ., match(., names(x)))
        }
        ## - check rows
        if(!harmonize_is_ok_type(rows, harmonize_data_length(x), type = "logical")) {
            rows <- TRUE  # select all if rows NULL
        }
    }, envir = parent.frame())
}
## --------<<  harmonize.x:1 ends here



## -------->>  [[file:../harmonizer.src.org::*harmonize.x.dots][harmonize.x.dots:1]]
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



## -------->>  [[file:../harmonizer.src.org::*old][old:1]]
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
            escape_regex_for_types(patterns.type.vector, escape_fixed = TRUE)
    }, envir = env)
}

harmonize.detect..get.codes.vector <- function(env = parent.frame()) {
    evalq({
        if(patterns.as.codes) patterns.vector
        else if(harmonize_is_ok_type(codes
                                   , x.length = c(1, harmonize_data_length(patterns))
                                   , type = "atomic")) {
            if(length(codes) == 1)    
                rep(defactor(codes), harmonize_data_length(patterns))
            else defactor(codes)
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
          unlist_if_possible
    }, envir = env)
}
## --------<<  old:1 ends here



## -------->>  [[file:../harmonizer.src.org::*harmonize][harmonize:1]]
##' Harmonizes organizational names. Takes either vector or column in the table.
##' 
##' @param x object (table)
##' @param procedures Named list of procedures (closures) to apply to x. If we need to pass arguments to some of the procedures it can be done by specifying sub-list where the first element is procedure and the rest its arguments. Names of the list elements are used for progress messages. Procedures can also be passed as data.frame in which case it will be converted to list of procedures with `harmonize_make_procedures_list` (see its help for the correct format of data.frame with procedures). Default is `harmonizer_default_procedures_table`
##' @param show_progress 
##' @param nrows_min_to_show_progress The minimum number of rows the x should have for automatic progress estimation. If x has less rows no progress will be shown. Default is 10^5
##' @param progress_step_nrows If set it will divide the x into chunk of this amount of rows. Default is NULL.
##' @param progress_step_in_percent Number of percents that represent one step in progress. Value should be between 0.1 and 50. Default is 1 which means it will try to chunk the x into 100 pieces.
##' @param progress_message_use_names Should we use names from `procedures` list to report progress. Default is TRUE.
##' @param quite Suppress all messages. Default is FALSE.
##' @param save_intermediate_x_to_var For debuging of standartization procedures. Saves intermediate results to this variable. If procedures finish without errors then the variable will be removed.
##' @param progress Show the progress? Default is TRUE
##' @inheritDotParams harmonize_options
##' 
##' @return
##'
##' @import stringi stringr magrittr
##' @export
harmonize <- function(x
                    , procedures = harmonizer_default_procedures_table
                    , show_progress = TRUE
                    , nrows_min_to_show_progress = 10^5
                    , progress_step_nrows = NULL
                    , progress_step_in_percent = 1
                    , progress_message_use_names = TRUE
                    , quite = FALSE
                    , save_intermediate_x_to_var = NULL
                    , ...) {
    checkmate::assert_string(save_intermediate_x_to_var, null.ok = TRUE)
    checkmate::assert_flag(show_progress)
    checkmate::assert_flag(quite)
    checkmate::assert_flag(progress_message_use_names)
    checkmate::assert_multi_class(procedures, classes = c("list", "data.frame"))
    if(is.data.frame(procedures)) {
        procedures <- harmonize_make_procedures_list(procedures)
    }
    ## make format of the massages for procedures
    message_delimiter <- paste(c("\n", rep("-", 65), "\n"), collapse = "")
    message_init <- paste0("\nApplying harmonization procedures:", message_delimiter)
    message_done  <- "\b\b\b\bDONE"
    progress_format <- "\b\b\b\b%3.0f%%"
    message_format <- "* %-60.60s...."
    message_fin <- paste0(message_delimiter, "Harmonization is done!\n")
    ## ensure that x is either vector or data.table
    if(missing(x)) return(NULL)
    checkmate::assert_multi_class(x
                                , classes = c("character"
                                            , "numeric"
                                            , "integer"
                                            , "logical"
                                            , "data.frame"
                                            , "data.table"))
    ## check progress_step_in_percent
    checkmate::assert_number(progress_step_in_percent, lower = 0.1, upper = 50)
    checkmate::assert_number(progress_step_nrows
                           , lower = x_length(x)/1000
                           , upper = x_length(x)/2
                           , null.ok = TRUE)
    x_len <- x_length(x)
    ## Set progress_step_nrows
    progress_step_nrows <-
        if (show_progress && !quite) {
            if(x_len < nrows_min_to_show_progress) {
                NULL
            } else if(!is.null(progress_step_nrows)) {
                progress_step_nrows
            } else {
                round(x_len / (100 / progress_step_in_percent))
            }
        }
    ## Apply Procedures
    if(!quite) message(message_init)
    for(p in 1:length(procedures)) {
        ## get procedure function
        procedure_fun <- procedures[[p]][[1]]
        ## get procedure arguments (remove show_progressarg if it is there)
        procedure_args <- procedures[[p]][
            -c(1, which(names(procedures[[p]]) == "show_progress"))
        ]
        ## get procedure names
        procedure_name <- 
            if(harmonize_is_data_empty(names(procedures)[p]) | !progress_message_use_names) {
                procedure_fun
            } else {
                names(procedures)[p]
            }
        ## Anounce Procedure Name
        if(!quite) packageStartupMessage(
                       sprintf(message_format, procedure_name)
                     , appendLF = FALSE)
        ## Check if we need report progress:
        ## progress is set & progress = FALSE is absent in the arguments
        if(!is.null(progress_step_nrows) &
           !isFALSE(procedures[[p]]["show_progress"][[TRUE]])) {
            ## check if we need to split..
            if(!isTRUE(class(x) == "list")) {
                x <- harmonize_x_split(x, progress_step_nrows)
            }
            ## set progress counter
            i <- 0; env <- environment()
            ## Apply procedure to list!
            x <- lapply(x, \(x_by) {
                if(!is.null(save_intermediate_x_to_var))
                    assign(save_intermediate_x_to_var, x, pos = 1)
                ## apply procedure fun with args
                x_by <- do.call(procedure_fun
                              , c(list(x_by), procedure_args))
                ## Increment progress counter
                assign("i", i + 100 * progress_step_nrows / x_len, envir = env)
                ## Anounce progress
                packageStartupMessage(
                    sprintf(progress_format, i)
                  , appendLF = FALSE)
                return(x_by)
            })
        } else {
            ## check if we need to rbindlist..
            if(isTRUE(class(x) == "list")) {
                if(is.atomic(x[[1]])) {
                    x <- unlist(x, use.names = FALSE)
                } else {
                    x <- rbindlist(x)
                }
            }
            if(!is.null(save_intermediate_x_to_var))
                assign(save_intermediate_x_to_var, x, pos = 1)
            ## Apply procedure fun with args!
            x <- do.call(procedure_fun, c(list(x), procedure_args))
        }
        ## Anounce DONE
        if(!quite) packageStartupMessage(message_done)
    }
    if(!quite) message(message_fin)
    ## return x
    if(isTRUE(class(x) == "list")) {
        if(is.atomic(x[[1]])) {
            x <- unlist(x, use.names = FALSE)
        } else {
            x <- rbindlist(x)
        }
    }
    ## remove intermediate saves if procedures finished without error
    remove
    if(!is.null(save_intermediate_x_to_var))
        rm(list = save_intermediate_x_to_var, pos = 1)
    return(x)
}
## --------<<  harmonize:1 ends here


