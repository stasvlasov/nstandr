## convert from factors
harmonize.defactor.vector <- function(x, check.numeric = TRUE) {
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

## Test
## factor(sample(c("a", "b", "b"), 20, replace = TRUE)) %>% harmonize.defactor.vector


harmonize.defactor <- function(x, ...) {
  if(is.atomic(x))
    harmonize.defactor.vector(x, ...)
  else if(is.matrix(x))
    as.matrix(lapply(x, harmonize.defactor.vector, ...))
  else if(is.data.table(x))
    as.data.table(lapply(x, harmonize.defactor.vector, ...))
  else if(is_tibble(x))
    as_tibble(lapply(x, harmonize.defactor.vector, ...))
  else if(is.data.frame(x))
    as.data.frame(lapply(x, harmonize.defactor.vector, ...)
                , stringsAsFactors = FALSE)
  else if(is.list(x)) 
    lapply(x, harmonize.defactor.vector, ...)
  else x
}

## Tests
## data.frame(num = factor(sample(runif(5), 20, replace = TRUE))
##          , let = factor(sample(c("a", "b", "b"), 20, replace = TRUE))) %>%
##   harmonize.defactor %>%
##   extract2("num")

## Get x.vector from x object which could be either verctor or table..
  ## if x.vector is provided do the opposite - return x object with x.vector incerted to it
#' @param x.col Which column to use for replacing
#' @param x.rows Logical vector to filter records to harmonize. Default is NULL which means do not filter records 
#' @param x.rows.col Column that indicates which records to harmonize. If set x.rows is ignored
#' @param x.vector.name If x is vector use this name for original column if it is in results. Default is "x". If x is table the name of x.col will be used.
#' @param harmonized.omitted.val If x.rows or x.rows.col is set. Use this value to fil NA
#' @param harmonized.omitted.col Update values in this column if x.rows or x.rows.col is set. If set harmonized.omitted.val is ignored
#' @param harmonized.name Use this name for the first column in results (harmonized names). Default is NULL, which menas that either x.vector.name if x is vector or original x.col name will be used is suffix harmonized.sufix.
#' @param harmonized.suffix If harmonized.name is not set the use "harmonized" as sufix
#' @param harmonized.suffix.update Unless orgizinal x.col columnt is returned updates suffix if there is one. Default is TRUE
#' @param return.x.cols If x is table, set the columns to cbind to the result table. Default is -1, meaning cbind all but the first (original/unharmonized) column.
#' @param return.x.cols.all Whether to bind all columns in x. Default is FALSE. If set the return.x.cols is ignored
  harmonize.x <- function(x
                        , x.inset = NULL
                        , x.col = 1
                        , x.rows = NULL
                        , x.rows.col = NULL
                        , x.vector.name = "x"
                        , harmonized.omitted.val = NA
                        , harmonized.omitted.col = NULL
                        , harmonized.name = NA
                        , harmonized.suffix = "harmonized"
                        , harmonized.suffix.update = TRUE
                        , return.x.cols = -x.col
                        , return.x.cols.all = FALSE) {
    x.is.atomic <- is.atomic(x)
    x.length <- if(x.is.atomic) length(x) else nrow(x)
    ## check x.col
    if(length(x.col) != 1)
      stop("x.col should be of length 1")
    if(!is.numeric(x.col) & !is.character(x.col))
      stop("x.col should be ethier numeric or character")
    ## if nothing was provides as x.vector then make and return one
    if(is.null(x.inset)) {
      ## ------------------------------
      ## check x.rows.col
      if(!is.null(x.rows.col)) {
        ## check if x[[x.rows.col]] is logical
        if(all(is.logical(x[[x.rows.col]]), na.rm = TRUE))
          x.rows <- x[[x.rows.col]]
        else stop("x[[x.rows.col]] should be logical type column!")
      }
      ## check x.rows
      if(!is.null(x.rows)) {
        ## check if x.rows is logical
        if(is.logical(x.rows)) {
          ## check if x.rows has different length as x
          if(is.logical(x.rows) & length(x.rows) != x.length)
            stop("x.rows has different length as x (length/nrow)!")
          ## check whether all x.rows are FALSE
          else if(all(!x.rows))
            return(x)
        } else stop("x.rows should be logical type!")
      }
      ## get vector to harmonize
      x %>%
        {if(x.is.atomic) . else .[[x.col]]} %>% 
        {if(is.null(x.rows)) . else .[x.rows]} %>%
        harmonize.defactor %>% return()
      ## ------------------------------
    } else {  # if x.inset is provided
      ## ------------------------------
      x.width <- if(x.is.atomic) 1 else ncol(x)
      x.names <- if(x.is.atomic) x.vector.name else names(x)
      ## check harmonized.omitted.col
      if(!is.null(harmonized.omitted.col))
        if(length(harmonized.omitted.col) != 1)
          stop("harmonized.omitted.col is wrong type, should be length 1")
        else if(x.is.atomic & harmonized.omitted.col != 1)
          stop("x is vector so the harmonized.omitted.col could only be 1")
        else if(is.numeric(harmonized.omitted.col) & harmonized.omitted.col > x.width)
          stop("Do not have harmonized.omitted.col in x. Check ncol(x).")
        else if(!is.numeric(harmonized.omitted.col) & !(harmonized.omitted.col %in% x.names))
          stop("Do not have harmonized.omitted.col in x. Check names(x).")
      ## harmonize.defactor and convert to data.table
      x %<>% {if(x.is.atomic) harmonize.defactor(.)
              else harmonize.defactor(as.data.table(.))}
      ## TODO: check return.x.cols...
      ## set return.x.cols
      if(is.null(return.x.cols)) return.x.cols <- 0
      ## set names
      x.vector.name %<>%
        {if(x.is.atomic) . else names(x[,..x.col])}
      harmonized.name %<>%
        {if(is.na(.)) {
           if(return.x.cols.all | any(return.x.cols == 1))
             x.vector.name %>% 
               paste0(".", harmonized.suffix)
           else
             x.vector.name %>%
               str_remove("\\.[^.]*$") %>%
               paste0(".", harmonized.suffix)
           ## TODO: check names..
           ## TODO: add indexes to harmonized... e.g. x.harmonized.2
         } else .}
      ## inset filtered rows
      x.inset %>% 
        {if(!is.null(x.rows))
           if(!is.null(harmonized.omitted.col))
             if(x.is.atomic)
               inset(x, x.rows, .)
             else
               inset(x[[harmonized.omitted.col]], x.rows, .)
           else
             inset(rep(harmonized.omitted.val, x.length), x.rows, .)
         else .} %>% 
        ## bind to existing table
        {if(return.x.cols.all |
            (x.is.atomic &
             ifelse(length(return.x.cols) == 1, return.x.cols == 1, FALSE)))
           cbind(data.table(.), x) %>%
             setnames(c(harmonized.name, x.names))
         else if(x.is.atomic) .
         else cbind(data.table(.), x[,..return.x.cols]) %>% 
                setnames(c(harmonized.name, x.names[return.x.cols]))
        } %>% return()
      ## ------------------------------
    }
  }


## ## tests
## ## ------------------------------
## data.table(c(1,2,3,4)
##          , c(7,8,9,0)) %>%
##   harmonize.x(c(5,5,5)
##             , x.rows = c(T,T,F,T))

## data.frame(c(1,2,3,4)
##          , c("7","8","9","a")) %>%
##   harmonize.x(x.col = 2
##             , x.rows = c(T,T,F,T))

## Removes redundant whitespases
#' @import stringr
harmonize.squish.spaces <- function(x, ...) {
  harmonize.x(x, ...) %>% # get x.vector
    str_squish %>%
    harmonize.x(x, ., ...) # put x.vector to x
}

harmonize.toupper <- function(x, ...) {
  harmonize.x(x, ...) %>% 
    toupper %>% 
    harmonize.x(x, ., ...)
}

## Tests
## data.table(name = c("MÄKARÖNI ETÖ FKÜSNÖ Ltd"
##                   , "MSLab CÖ. <a href=lsdldf> <br> <\\a>"
##                   , "MSLab Co."
##                   , "MSLaeb Comp."
##                   , "MSLab Comp."
##                   , "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝŸ") %>%
##              rep(10)
##          , foo = "lalala" ) %>% harmonize.toupper

## Removes brackets and content in brackets
#' @import stringr
harmonize.remove.brackets  <- function(x, ...) {
  harmonize.x(x, ...) %>% 
    str_replace_all("<.*>|\\(.*\\)|\\{.*\\}|\\[.*\\]", "") %>%
    harmonize.x(x, ., ...)
}



## remove.brackets breaks the encoding
## harmonize.remove.brackets("fa\xE7ile (lalala)")

## Removes double quotes
## (It is a separate procedure because read.csv can not get this substitution)
#' @import stringr
harmonize.remove.quotes <- function(x, ...) {
  harmonize.x(x, ...) %>% 
    stri_replace_all_fixed("\"", "") %>% 
    harmonize.x(x, ., ...)
}

## Escapes special for regex characters
harmonize.escape.regex <- function(string) str_replace_all(string, "(\\W)", "\\\\\\1")
## escape.regex  <- function (string) {
##   gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", string)
## }


## Escapes special for regex characters conditionaly
harmonize.escape.regex.cond <- function(strings, conds) {
  mapply(function(string, cond) {
    if(cond == "fixed") harmonize.escape.regex(string)
    else if(cond == "begins") paste0("^", harmonize.escape.regex(string))
    else if(cond == "ends") paste0(harmonize.escape.regex(string), "$")
    else if(cond == "regex") string
  }
, strings
, conds
, SIMPLIFY = TRUE)
}

## Test escape.regex.cond
## c("MSlab$", "TriloBit.?", "(^0-3)", "Ltd.") %>%
##   escape.regex.cond(c("regex", "fixed", "regex", "ends"))

## Removes elements that are either "", NA, NULL or have zero length
harmonize.is.empty <- function(xs) {
  lapply(xs, function(x) {
    ifelse(length(x) == 0, TRUE, all(x == "" | is.na(x)))
  }) %>% unlist(recursive = FALSE)
}

## list("INCORPORATED", NULL, NULL, NULL, NULL) %>% is.empty
## c(NA, "", 3,4, "wsd", NULL) %>% is.empty

harmonize.empty.omit <- function(x) {
  x[!sapply(harmonize.is.empty(x), isTRUE)]
}

## test
## list("INCORPORATED", NULL, NULL, NULL, NULL) %>% empty.omit

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


## Tests
## c(1,2,3,4) %>% harmonize.unlist.column
## list(c("a"), NULL, 3, "5", character(0)) %>% harmonize.unlist.column
## list(c("a"), 3, "5") %>% harmonize.unlist.column
## list(c("a", "b", "c"), NULL, 3, "5", character(0)) %>% harmonize.unlist.column

harmonize.split.x <- function(x, by, len) {
  split(x, rep(seq(1, len %/% by +1)
             , each = by
             , length.out = len))
}

harmonize.how.long <- function(x) {
  if(is.atomic(x)) length(x) else nrow(x)
}

## Encodes as UTF-8
#' @import stringr
toutf <- function(str) str_conv(str, "UTF-8")

#' @import stringi magrittr
detect.enc <- function(str) {
    stri_enc_detect(str) %>% lapply(function(enc) {
        enc %<>% extract2("Encoding")
        first.ok.enc <- enc %in% harmonizer.enc.list %>% which %>% extract(1)
        if(length(first.ok.enc) == 0) ""
        else enc[first.ok.enc]
    }) %>% unlist %>% return
}


#' Translates non-ascii symbols to its ascii equivalent
#'
#' It takes characters from this string:
#' ŠŒŽšœžŸ¥µÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝßàáâãäåæçèéêëìíîïðñòóôõöøùúûüýÿ
#' And translates to this one
#' SOZsozYYuAAAAAAACEEEEIIIIDNOOOOOOUUUUYsaaaaaaaceeeeiiiionoooooouuuuyy
#' @param str String to translate
#' @param detect.encoding Detect encoding of individual elements
#' @import stringi stringr magrittr
#' 
#' @export
toascii <- function(str, detect.encoding = FALSE) {
    ## utf <- "ŠŒŽšœžŸ¥µÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝßàáâãäåæçèéêëìíîïðñòóôõöøùúûüýÿ" %>% toutf
    utf <- harmonization.accented
    ascii <- "SOZsozYYuAAAAAAACEEEEIIIIDNOOOOOOUUUUYsaaaaaaaceeeeiiiionoooooouuuuyy"
    if(detect.encoding)  # detect encoding of individual elements
        mapply(function(name, enc)
            iconv(name
                , from = enc
                , to = "UTF-8"
                , sub = "") %>%
            {chartr(utf, ascii, .)}
          , str
          , detect.enc(str)
          , SIMPLIFY = FALSE, USE.NAMES = FALSE) %>%
            unlist %>% 
            iconv(to = "ASCII", sub = "")
    else
        enc2utf8(str) %>% 
            {chartr(utf, ascii, .)} %>% 
            iconv(to = "ASCII", sub = "")
}


## Test
## detect.enc(c("FAÇILE"
##         , "fa\xE7ile"
##         , "c\u00b5c\u00b5ber"))

## toascii(c("FAÇILE"
##         , "fa\xE7ile"
##         , "c\u00b5c\u00b5ber")
##         , detect.encoding = TRUE)

## Convert HTML characters to UTF-8 (this one is 1/3 faster than htmlParse but it is still very slow)
## from - http://stackoverflow.com/questions/5060076
#' @import xml2 magrittr 
html2txt <- function(strings) {
    sapply(strings, function(str) {
        if(!is.null(str)) {
            paste0("<x>", str, "</x>") %>%
                read_html %>%
                xml_text 
        } else {
            return(str)
        }
    }) %>% as.vector
}

#' A wrapper for string replacement and cbinding some columns.
#'
#' Optionally matches only at the beginning or at the end of the string.
#' 
#' @param x Vector or table to harmonize.
#' @param patterns Accepts both vector or table. If patterns it is table can also include replacements column.
#' @param patterns.col If patterns is not a vector which column to use. Default is 1.
#' @param patterns.type Kind of pattern. Default is "fixed" (calling code{\link[stringi]{stri_replace_all_fixed}}). Other options are "begins", "ends" - which means that it should only match fixed pattern at the beginning of the string or at the and. Another possible value is "regex" (calling code{\link[stringi]{stri_replace_all_regex}})
#' @param patterns.type.col 
#' @param patterns.replacements.col If patterns is not a vector and includes replacements which column to use for replacements. Default is 2.
#' @param replacements If patterns does not have column with replacements provide it here.
#' @param replacements.col If replacements is not a vector which column to use. Default is 1.
#' @param ... If replacements is not a vector which column to use. Default is 1.
#'
#' @return If nothing was indicated to cbind to results then it returns harmonized vector. If something is needs to be cbind then it returns data.table
harmonize.replace <- function(x
                            , patterns
                            , patterns.col = 1
                            , patterns.type = "fixed"
                            , patterns.type.col = NULL
                            , patterns.replacements.col = 2
                            , replacements = NULL
                            , replacements.col = 1
                            , ...) {
  ## check pattern type
  patterns.is.atomic <- is.atomic(patterns)
  patterns.type %<>% {if(length(.) == 1)
                        if(. %in% c("fixed", "begins", "ends", "regex")) .
                        else if(is.numeric(.)) patterns[[.]]
                        else if(!is.null(patterns[[.]])) patterns[[.]]
                        else stop("patterns.type misspecified!")
                      else if(length(.) == ifelse(is.null(nrow(patterns))
                                                , length(patterns)
                                                , nrow(patterns))) .
                      else stop("patterns.type misspecified!")}
  ## get replacesments vectors
  replacements %<>%
    {if (is.null(.)) if (patterns.is.atomic) ""
                     else patterns[[patterns.replacements.col]]
     else if (is.atomic(.)) .
     else .[[replacements.col]]}
  ## get replacesments patterns
  patterns %<>%
    {if (patterns.is.atomic) . else .[[patterns.col]]} %>%
    {if(length(patterns.type) == 1)
       if(patterns.type == "begins") paste0("^", harmonize.escape.regex(.))
       else if(patterns.type == "ends") paste0(harmonize.escape.regex(.), "$")
       else .
     else harmonize.escape.regex.cond(.,patterns.type)}
  ## harmonize
  ## ---------
  x.vector <- harmonize.x(x, ...)
  x.vector %<>% 
    {if(length(patterns.type) == 1 & patterns.type[1] == "fixed") {
       stri_replace_all_fixed(.
                            , patterns
                            , replacements
                            , vectorize_all = FALSE)
     } else {
       stri_replace_all_regex(.
                            , patterns
                            , replacements
                            , vectorize_all = FALSE)
     }}
  ## ---------
  ## inset x.vector
  harmonize.x(x, x.vector, ...) %>% return()
}


## Test harmonize.replace
## data.frame(x.lala = c("lala MSlab"
##                , "this company called TriloBit.? maybe"
##                , "MS007lab, Ltd.")
##          , x.rows = c(TRUE, TRUE, FALSE)
##          , harm = c(1,2,"MSlab")) %>%
##   harmonize.replace(patterns = c("MSlab$", "TriloBit.?", "[0-3]*", "Ltd.")
##                   , patterns.type = c("regex", "fixed", "regex", "ends")
##                   , harmonized.omitted.col = 3
##                   , x.rows = c(TRUE, TRUE, FALSE)
##                   , return.x.cols = 3
##                   )

## this function is basically for coding names based on certain pattern

#' A wrapper for string replacement and cbinding some colums.
#'
#' Optionaly matches only at the beginning or at the end of the string.
#' 
#' @param x Vector or table to detect in.
#' @param patterns Accepts both vector or table. If patterns it is table can also include replacements column.
#' 
#' @param patterns.col If patterns is not a vector specifies which column to use. Defauld is 1.
#' @param patterns.type Kind of pattern. Default is "fixed" (calling code{\link[stringi]{stri_replace_all_fixed}}). Other options are "beggins", "ends" - which means that it should only match fixed pattern at the beginngin of the string or at the and. Another possible value is "regex" (calling code{\link[stringi]{stri_replace_all_regex}})
#' @param patterns.codes.col If patterns is table which column to use as codes column.
#' 
#' @param codes If provided use it as codes. Should be the same length as patterns
#' @param codes.col If codes is not vector use this column for codes
#' @param codes.name If provided use it as a name for codes column in results.
#' @param codes.suffix If codes.name is not provided use this suffix to x.col name or x.vector.name if x is vector
#' @param codes.first If TRUE then return only codes for the first detected pattern. Otherwise return list of all matched codes. Default is FALSE.
#' 
#' @param x.col Which column to detect in. Default is 1
#' @param x.vector.name If x was vector then use this name as a column name. Otherwise original column names will be used.
#' @param x.rows Filter which records (rows) to detect in. Default is NULL which means detect in all records (rows)
#' @param x.codes.col If x is table, which column to use for making/merging/adding newly detected codes. Default is last column of x or NULL is x is vector
#' @param x.codes.add If set then detect and add new codes only for records (rows) that were not yet coded (i.e., related codes are either "", NA or length == 0).
#' @param x.codes.merge If set then merge (append) new codes to existing one.
#' @param return.just.codes If set then just return codes vector. Default is FALSE.
#' 
#' @return If nothing was indicated to cbind to resutls then it returns harmonized vector. If something is needs to be cbind then it returns data.table
harmonize.detect <- function(x
                           , patterns
                           , patterns.col = 1
                           , patterns.type = "fixed"
                           , patterns.codes.col = 2
                           , codes = NULL
                           , codes.col = 1
                           , codes.name = NA
                           , codes.suffix = "code"
                           , codes.first = FALSE
                           , x.col = 1
                           , x.vector.name = "x"
                           , x.rows = NULL
                           , x.codes.col = ncol(x)
                           , x.codes.add = FALSE
                           , x.codes.merge = FALSE
                           , return.just.codes = FALSE) {
  ## set vector to detect in
  x.is.atomic <- is.atomic(x)
  x %<>% {if(x.is.atomic) . else as.data.table(.)} %>% harmonize.defactor
  patterns %<>% harmonize.defactor
  ## set existing codes vector
  x.codes <- if(!x.is.atomic) x[[x.codes.col]] else NULL
  ## filter x.rows or if x.codes.add is set filter those that have codes already
  if((x.codes.add & !x.is.atomic) | !is.null(x.rows)) {
    x.rows <- x.codes %>% harmonize.is.empty
    ## if all x.rows are FALSE so anything add and just return original
    if(all(!x.rows)) return(x)
  }
  x.vector <- x %>%
    {if(x.is.atomic) . else .[[x.col]]} %>%
    {if(is.null(x.rows)) . else .[x.rows]}
  ## set codes column name
  codes.name %<>%
    {if(!is.na(.)) .
     else names(patterns)[patterns.codes.col] %>% 
            {if(!is.null(.)) .
             else names(x)[x.col] %>%
                    {if(!is.null(.)) paste0(., ".", codes.suffix)
                     else paste0(x.vector.name, ".", codes.suffix)}}}
  ## check existing codes
  codes %<>%
    {if(!is.null(codes)) .
     else patterns %>%
            {if(is.atomic(.)) .
             else .[[patterns.codes.col]]}}
  ## set patterns
  patterns %<>%
    {if(is.atomic(.)) .
     else .[[patterns.col]]} %>% 
    {if(patterns.type == "begins")
       paste0("^", harmonize.escape.regex(.))
     else if(patterns.type == "ends")
       paste0(harmonize.escape.regex(.), "$")
     else .}
  ## do detection
  mapply(
    function(pattern, code) {
      x.vector %>%
        {if(patterns.type == "fixed")
           stri_detect_fixed(., pattern)
         else
          stri_detect_regex(., pattern)} %>% 
        ifelse(code, NA) %>%
        ## remove empty string ("") codes
        ifelse(. == "", NA, .)
    }
  , patterns
  , codes
  , SIMPLIFY = FALSE, USE.NAMES = FALSE) %>%
    ## transpose list of vectors
    {do.call(mapply, c(c, ., SIMPLIFY = FALSE, USE.NAMES = FALSE))} %>% 
    lapply(na.omit) %>%
    ## check if only first detected code is needed
    {if(codes.first) lapply(.,extract, 1) else .} %>%
    ## check if we need to merge
    {if(x.codes.merge & !is.null(x.codes))
       mapply(function(a,b) c(b, a)
            , .
            , x.codes[if(is.null(x.rows)) TRUE else x.rows]
            , SIMPLIFY = FALSE)
     else .} %>%
    ## remove empty codes
    lapply(harmonize.empty.omit) %>% 
    ## inset records and codes that were filtered out if it was a case
    {if(!is.null(x.rows))
       if(x.codes.add & !is.null(x.codes.col))
         inset(x.codes, x.rows, .)
       else
         inset(rep(NA, length(x[[x.codes.col]])), x.rows, .)
     else .} %>%
    ## unlist column if possible
    harmonize.unlist.column %>% 
    ## return codes
    {if(return.just.codes) .
     else if(x.codes.merge | x.codes.add) {
       x[[x.codes.col]] <- .
       return(x)
     } else {
       ## if x did not have codes column add it at the end
       cbind(x, data.table(.)) %>%
         setnames(c(colnames(x) %>% {if(is.null(.)) x.vector.name else .}
                  , codes.name)) %>%
         return()
     }
    }
}

## Tests

data.frame(
  name =   c("MSlab Co."
           , "IBM Corp."
           , "Tilburg University")
, codes = c("",3,NA)) %>%
  harmonize.detect(c("Co.", "Corp.", "MS")
                 , patterns.type = "ends"
                 , x.codes.merge = TRUE)

c("MSlab Co."
, "IBM Corp."
, "Tilburg University") %>% 
  harmonize.detect(data.table(c("Co.", "Co"), type = c("corp", "corp2"), some.extra.col = c(1,2)))


c("MSlab Co."
, "IBM Corp."
, "Tilburg University") %>% 
  harmonize.detect(data.frame(c("Co.", "Co")
                            , type = c("corp", "corp2"))
                 , codes.first = TRUE
                 , patterns.type = "ends")



data.frame(c("Co.", "Co")
         , type = c("corp", "corp2")) %>%
  harmonize.defactor %>%
  extract2("type")

## appling list of procedures
 ## --------------------------------------------------------------------------------

 ## Iterative function application:

 ## Reduce implementation
 ## fun.call <- function(f, ...) {
 ##   if(is.list(f)) do.call(f[[1]], c(..., f[-1]))
 ##   else f(...)
 ## }
 ## pipe.fun.list <- function(x, fun.list) {
 ##   Reduce(fun.call, rev(fun.list), x, right = TRUE)
 ## }

 ## bare-bones implementation
 pipe.fun <- function(x, ...)
 {
   fun.list  <- list(...)
   for(i in 1:length(fun.list)) {
       fun <- fun.list[[i]]
       if (is.list(fun)) x <- do.call(fun[[1]], list(x, fun[[-1]]))
       else x <- fun(x)
   }
   return(x)
 }

pipe.fun.list <- function(x, fun.list)
 {
   for(i in 1:length(fun.list)) {
       fun <- fun.list[[i]]
       if (is.list(fun)) x <- do.call(fun[[1]], c(list(x), fun[-1]))
       else x <- fun(x)
   }
   return(x)
 }



 ## Tests:
 c(0.3) %>% 
     pipe.fun.list(fun.list = list(sum, sqrt, list(log, 10), abs))


one.p.one <- function(x) x+1+1

math.fun.list  <- list(sum, sqrt, list(log, 10), abs, one.p.one)


 c(0.3) %>% 
     pipe.fun.list(math.fun.list)

## experiments with just ...
## I do not need that. List is better
## {function(x, ...) length(deparse(...))}(0, sum, sqrt)

harmonize <- function(x
                    , procedures = harmonize.default.procedures
                      ## , procedures.message = c("list.name", "name", "doc.title")
                    , progress = TRUE
                    , progress.min = 10^5
                    , progress.by = NA
                    , progress.percent = 1
                    , quite = FALSE
                    , ...) {
  ## make format of the massages for procedures
  message.format <- "* %-60.60s...."
  progress.format <- "\b\b\b\b%3.0f%%"
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
                   x.length <- if(is.atomic(x)) length(x) else nrow(x)
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
  if(!quite) message("Applying harmonization procedures:")
  for(procedure in procedures) {
    ## get procedure fun and args
    procedure.fun <- procedure %>%
      extract2(1)
    procedure.args <- procedure %>%
      ## remove progress arg if it is there
      extract(-c(1, which(names(.) == "progress")))
    ## Anounce Procedure Name
    if(!quite) packageStartupMessage(sprintf(message.format, procedure.fun)
                                   , appendLF = FALSE)
    ## Check if we need report progress:
    ## progress is set & progress = FALSE is absent in the arguments
    if(!is.na(progress.by) & !isFALSE(procedure["progress"][[TRUE]])) {
      ## check if we need to split..
      if(isTRUE(class(x) != "list")) {
        x %<>% harmonize.split.x(progress.by, x.length)
      }
      ## set counter
      i <- 0; env <- environment()
      ## Apply procedure to list!
      x %<>% lapply(function(x.by) {
        ## apply procedure fun with args
        x.by %<>%
          list %>%
          c(procedure.args) %>%
          do.call(procedure.fun, .)
        ## Anounce progress
        assign("i", i + 100*progress.by/x.length, envir = env)
        packageStartupMessage(sprintf(progress.format, i), appendLF = FALSE)
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
    if(!quite) packageStartupMessage("\b\b\b\bDONE")
  }
  if(!quite) message("Harmonization is done!\n")
  ## Return X
  if(isTRUE(class(x) == "list")) {
    if(is.atomic(x[[1]])) x %>% unlist(use.names = FALSE)
    else x %>% rbindlist
  } else x
}


## tests
## dummy <- function(x, n) {
##   for(i in 1:n) x <- sqrt(x)^2
##   return(x)
## }

## harmonize(1:10^2, list("sqrt",list("abs", progress = FALSE),list("log", base = 10), list("dummy", 10^6, progress = FALSE))
##         , progress.min = 10
##         , progress.by = 30)
