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
## from - http://stackoverflow.com/questions/5060076/convert-html-character-entity-encoding-in-r?lq=1
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

## Removes redundant whitespases
#' @import stringr
remove.spaces <- function(strings) {
    gsub("\\s+", " ", stringr::str_trim(strings))
}

## Removes brackets and content in brackets
#' @import stringr
remove.brackets  <- function(str) str_replace_all(str, "<.*>|\\(.*\\)|\\{.*\\}|\\[.*\\]", "")

## remove.brackets breaks the encoding
## remove.brackets("fa\xE7ile")

## Removes double quotes
## (It is a separate procedure because read.csv can not get this substitution)
#' @import stringr
remove.quotes <- function(str) stri_replace_all_fixed(str, "\"", "")

## Subsitutes strings
#' @import magrittr stringi stringr data.table
apply.harmonization <- function(org.names
                              , harmonization.names
                              , add.spaces = "both"
                              , del.spaces = TRUE
                              , regex = FALSE) {
    if(del.spaces) org.names %<>% remove.spaces
    if(add.spaces == "right")
        org.names %<>%  paste0(" ")  # add space at the end
    if(add.spaces == "both")
        org.names %<>% {paste0(" ",. , " ")}  # add space at the end
    harmonization.tab <-
        harmonization.tabs[harmonization.names] %>%
        rbindlist
    if(regex) org.names %<>% stri_replace_all_regex(harmonization.tab$del
                                                  , harmonization.tab$ins
                                                  , vectorize_all = FALSE)
    else org.names %<>% stri_replace_all_fixed(harmonization.tab$del
                                             , harmonization.tab$ins
                                             , vectorize_all = FALSE)
    return(org.names)
}

## this should accept both vector and table and return eather vector or a table
harmonize.replace <- function(x
                            , patterns
                            , replacements = NULL
                            , replacements.col = 2
                            , harmonize.col = 1
                            , bind.x.cols = c("none"
                                            , "harmonize.col"
                                            , "all.but.harmonize.col"
                                            , "all")[1]
                            , bind.patterns.cols = NULL) {
  patterns.is.vector <- is.vector(patterns)
  replacements %<>%
    {if (is.null(.)) if (patterns.is.vector) "" else patterns[[replacements.col]]
     else if (is.vector(.)) .
     else .[[1]]}
  patterns %<>% {if (patterns.is.vector) . else .[[1]]}
  x.is.vector <- is.vector(x)
  x %>%
    {if(x.is.vector) . else .[[harmonize.col]]} %>%
    stri_replace_all_fixed(patterns
                         , replacements
                         , vectorize_all = FALSE) %>%
    {if(bind.x.cols == "none" | (x.is.vector & bind.x.cols == "all.but.harmonize.col")) .
     else if(x.is.vector & (bind.x.cols == "all" | bind.x.cols == "harmonize.col"))
       cbind(harmonized = ., data.table(original = x))
     else if(bind.x.cols == "all")
       cbind(harmonized = ., as.data.table(x))
     else if(bind.x.cols == "all.but.harmonize.col")
       cbind(harmonized = ., as.data.table(x) %>% select(-harmonize.col))
     else if(bind.x.cols == "harmonize.col")
       cbind(harmonized = ., as.data.table(x) %>% select(harmonize.col))
    }
}

## Apply Magerman Example:
## , magerman.remove.common.words
## , magerman.clean.punctuation
## , list(magerman.do.something, col = 2)
## , magerman.clean.punctuation

## this function is basically for coding names based on certain pattern
harmonize.detect <- function(x
                           , patterns
                           , x.col = 1
                           , patterns.col = 1
                           , type = c("fixed", "regex")[1]
                           , bind.patterns.col = 2
                           , bind.patterns.col.name = NA) {
  ## x.is.vector <- is.vector(x)
  x.vector <- x %>% {if(is.vector(.)) . else .[[x.col]]}
  bind.patterns.col.name %<>% {if(!is.na(.)) .
                               else names(patterns)[bind.patterns.col] %>% 
                                      {if(!is.null(.)) .
                                       else names(x)[x.col] %>%
                                              {if(!is.null(.)) paste0(., ".detected")
                                               else "detected"}}}
  if(type == "fixed" | type == "regex") {
    mapply(function(pattern, bind) {
      x.vector %>%
        {if(type == "fixed") stri_detect_fixed(., pattern)
         else stri_detect_regex(., pattern)} %>%
        ifelse(bind, NA)
    }
  , patterns %>% {if(is.vector(.)) . else .[[patterns.col]]}
  , patterns %>% {if(is.vector(.)) . else .[[bind.patterns.col]]}
  , SIMPLIFY = TRUE
  , USE.NAMES = TRUE) %>%
    apply(1, na.omit) %>% 
    data.table %>%
    cbind(x,.) %>% 
    set_names(c(names(x) %>% ifelse(is.null(.),NA,.)
              , bind.patterns.col.name))
  } else message("Wrong type!")
}


## Tests

c("MSlab Co."
, "IBM Corp."
, "Tilburg University") %>% 
  harmonize.detect(data.table(c("Co.", "Co"), type = c("corp", "corp2")))


## here data.frame does the factoring for columns
c("MSlab Co."
, "IBM Corp."
, "Tilburg University") %>% 
  harmonize.detect(data.frame(c("Co.", "Co"), type = c("corp", "corp2")))


c("MSlab Co."
, "IBM Corp."
, "Tilburg University") %>% 
  harmonize.detect(c("Co.", "Co"))

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
 pipe.fun.list <- function(x, fun.list)
 {
   for(i in 1:length(fun.list)) {
       fun <- fun.list[[i]]
       if (is.list(fun)) x <- do.call(fun[[1]], c(x, fun[[-1]]))
       else x <- fun(x)
   }
   return(x)
 }

 ## Tests:
 c(0.3) %>% 
     pipe.fun.list(list(sum, sqrt, list(log, 10), abs))

## experiments with just ...
## I do not need that. List is better
## {function(x, ...) length(deparse(...))}(0, sum, sqrt)

## Harmonize Organizational Names
## This function combines all previous functions
## ================================================================================
#' @title Harmonize organizational names. 
#'
#' @description
#' Returns harmonized version of organizational names.
#' @param org.names Character vector of organizational names to harmonize
#' @param quite Logical value indicating whether or not print messages about procedures progress
#' @param include.original Logical value indicating whether or not include original vector of orgnamizational names (e.g., org.names). Default is FALSE. If TRUE the function will return data.frame instead of vector.
#' @param progress.by Numeric value that is used to split the org.names vector for showing percentage of completion. Default is 0 meaning not to split the vector and thus does not show progress percentage. Designed to be used for long strings.
#' @param procedures List of harmonization procedures. Each procedure can be specified as a string representing procedure name (see details for procedure names) or as a list where the first element should be procedure name (string) and other elements will passed as arguments to this procedure.
#' @return Character vector of harmonized  organizational names (if include.original = FALSE - default). If include.original = TRUE the function returns data.frame(original = org.names, harmonized = harmonize(org.names))
#' @import magrittr pbapply stringr stringi data.table xml2
#' @export
#' @examples
#' org.names.test <- c("žŸong-ÂÃÇÈÏa\n\u00b5 &oacute;\u00b5<p>,  INt LTD &AMP; Co;  "
#'                   , "<br> the $ (&AMP; C&oacute;MP comPANY) Ïotta")
#' data.frame(original = org.names.test
#'          , harmonized = harmonize(org.names.test))
#' @details The following procedures are available:
#' * toutf - encode as UTF-8 (a wrapper for str_conv(str, "UTF-8"))
#' * tolower - lowercase
#' * toupper - uppercase
#' * html2txt - removes HTML symbols and tags (relatively slow)
#' * toascii - replaces accented characters with ascii equivalent. Can also identify encoding for each character string in vector (option detect.encoding = TRUE). Does not detect encoding by default (detect.encoding = FALSE) and assumes that the right one is specified for the vector.
#' * remove.brackets - removes brackets and its content - (), [], {}, <>
#' * remove.spaces - removes double white spaces and trims white spaces
#' * remove.quotes - removes double quotes
#' * apply.nber - applies standard name substitutions following NBER's PDP (Thoma, et al, 2010) - https://sites.google.com/site/patentdataproject/Home/posts/namestandardizationroutinesuploaded
#' * apply.nber.sansremovals - same as apply.nber but without removals of organization legal form
#' * apply.nber.sansderwent - same as apply.nber but without Derwent names standardization
#' * apply.nber.sansderwent.sansremovals - same as apply.nber.sansderwent but without removals of organization legal form
#' * apply.punctuation - removes punctuation
#'
#' External functions can be also passed as a separate harmonizer procedure
#' @md
harmonize <- function(org.names
                    , procedures = list(
                          list("toascii", TRUE)
                        , "tolower"
                        , "html2txt"
                        , list("toascii", FALSE)
                        , "remove.brackets"
                        , "toupper"
                        , "apply.nber"
                        , "remove.spaces"
                      )
                    , quite = FALSE
                    , progress.by = 0
                    , include.original = FALSE
                      ) {
    ## ================================================================================
    ## check if procedures are specified
    if(length(procedures) == 0) {message("No harmonizing procedures are specified."); return()}
    if(include.original) org.names.original <- org.names
    ## Apply harmonization
    if(is.character(org.names)) {
        if(!quite) message("Running harmonizer procedures:")
        for(procedure in procedures) {
            if(!quite) packageStartupMessage("* ", procedure[[1]], " ..."
                                           , paste0(rep("\t"
                                                      , 5 - ((nchar(procedure[[1]]) + 6) %/% 8)))
                                           , appendLF = FALSE)
            if(progress.by & !quite) {
                env <- environment()
                i <- 1
                l <- length(org.names)
                n <- l %/% progress.by + 1
                packageStartupMessage("  0%"
                                    , appendLF = FALSE)
                org.names %<>%
                    split(rep(1:n
                            , each = progress.by
                            , length.out = l)) %>%
                    lapply(function(org.names.by) {
                        packageStartupMessage("\b\b\b\b", " "
                                            , ifelse(round(100/n * i) < 10 , " ", "")
                                            , round(100/n * i), "%"
                                            , appendLF = FALSE)
                        assign("i", i + 1, envir = env)
                        org.names.by %>% 
                            list %>%
                            c(procedure[-1])  %>%        # add arguents at the end
                            {do.call(procedure[[1]], .)}
                    }) %>% unlist
            }
            else org.names %<>% 
                     list %>%
                     c(procedure[-1])  %>%        # add arguents at the end
                     {do.call(procedure[[1]], .)}
            if(progress.by & !quite) packageStartupMessage("\b\b\b\b", "DONE")
            if(!progress.by & !quite) packageStartupMessage("DONE")
        }
        if(include.original) return(data.frame(original = org.names.original
                                             , harmonized = org.names))
        else return(org.names)
    } else {
        if(include.original) return(data.frame(original = org.names.original
                                             , harmonized = org.names))
        else return(org.names)
    }
}

## sequence used in NBER PDP
## ................................................................................
## - 0 leading and trimming spaces
## - 1 punctuation2
## - 2 derwent 
## - 3 standard_name 
## - 4 corporates
## - 5 (combabbrev) - it is just coding for legal form
## - 6 (stem_name) - I left common names untouched


## Define functions for harmonization
## ================================================================================
## Load Substitution Rules
## ================================================================================

## Path for files with substitutions
delayedAssign("inst.dir", system.file(package = "harmonizer"))
## inst.dir <- file.path("../inst")  # for testing
## Specify accented char to replace with ascii equivalents
## Note: a" wraper for delayedAssign() is %<d-% from pryr package
delayedAssign("harmonization.accented"
            , file.path(inst.dir, "ascii-equivalents/accented-chars.txt") %>% 
              readLines(encoding = "UTF-8") %>%
              enc2utf8)
## Specify folders to scan for CSV with substitutions
delayedAssign("harmonization.dirs"
            , c("nber-pdp-harmonization"
                ## , "magerman-harmonization"
              , "additional-harmonization"))
delayedAssign("harmonization.files"
            , (sapply(file.path(inst.dir, harmonization.dirs)
                    , list.files
                    , pattern = ".csv$", full.names = TRUE) %>% unlist))
delayedAssign("harmonization.names"
            , basename(harmonization.files) %>%
              str_replace(".csv$", ""))
## Load all CSVs into list
delayedAssign("harmonization.tabs"
            , lapply(harmonization.files, function(file) {
                ## packageStartupMessage("* Loading substitutions: "
                ##                     , basename(file))
                read.csv(file
                       , header = FALSE
                       , col.names = c("del", "ins")
                       , as.is = TRUE
                       , colClasses = c("character", "character")
                       , na.strings = NULL
                       , comment.char = "#"
                       , strip.white = TRUE
                       , encoding = "UTF-8")
            }) %>% setNames(harmonization.names))
## Get list of system encodings (should save time)
delayedAssign("harmonizer.enc.list", iconvlist())










## Test
## apply.harmonization("!s!df,.sdf,.sd LTD, CO", c("nber-punctuation"
##                                               , "nber-stem-name"))


## Wrapers for harmonizations
## ================================================================================
#' @import magrittr
apply.nber <- function(org.names) {
    org.names %>% 
        apply.harmonization(c("additional-name-only"
                            , "nber-nameonly")) %>% 
        apply.harmonization("nber-punctuation"
                          , add.spaces = FALSE
                          , del.spaces = FALSE) %>%
        remove.quotes %>% 
        apply.harmonization(c("additional-substitutions"
                            , "nber-standard-name"
                            , "nber-univ"
                            , "nber-derwent")) %>%
        apply.harmonization("additional-regex"
                          , regex = TRUE
                          , add.spaces = FALSE) %>% 
        apply.harmonization(c("additional-removals"
                            , "nber-stem-name")
                          , add.spaces = "right")
}


## Test
## apply.nber("LTD,LAL,LDF&^*($), &CO. LIMITED")


## I see that there is quite a lot of umlauts in harmonization replacements
## So it should be either at the end or more sophisticated
## str_subset(harmonization.tabs %>% rbindlist %>% extract2("ins"), "UE|AE|OE")
#' @import magrittr
apply.ulmaut <- function(org.names) {
    org.names %>% 
        apply.harmonization("nber-umlaut")
}


#' @import magrittr
apply.derwent <- function(org.names) {
    org.names %>% 
        apply.harmonization("nber-derwent")
}

#' @import magrittr
apply.nber.sansremovals <- function(org.names) {
    org.names %>% 
        apply.harmonization(c("additional-name-only"
                            , "nber-nameonly")) %>% 
        apply.harmonization("nber-punctuation"
                          , add.spaces = FALSE
                          , del.spaces = FALSE) %>%
        remove.quotes %>% 
        apply.harmonization(c("additional-substitutions"
                            , "nber-standard-name"
                            , "nber-univ"
                            , "nber-derwent")) %>% 
        apply.harmonization("additional-regex"
                          , regex = TRUE
                          , add.spaces = FALSE) 
}

#' it assumes that there is no removals for the first word
#' @import magrittr
apply.nber.removals <- function(org.names) {
    org.names %>% 
        apply.harmonization(c("additional-removals"
                            , "nber-stem-name")
                          , add.spaces = "right")
}


#' @import magrittr
apply.nber.sansderwent.sansremovals <- function(org.names) {
    org.names %>% 
        apply.harmonization(c("additional-name-only"
                            , "nber-nameonly")) %>% 
        apply.harmonization("nber-punctuation"
                          , add.spaces = FALSE
                          , del.spaces = FALSE) %>%
        remove.quotes %>% 
        apply.harmonization(c("additional-substitutions"
                            , "nber-standard-name"
                            , "nber-univ")) %>% 
        apply.harmonization("additional-regex"
                          , regex = TRUE
                          , add.spaces = FALSE)
}

#' @import magrittr
apply.nber.sansderwent <- function(org.names) {
    org.names %>% 
        apply.harmonization(c("additional-name-only"
                            , "nber-nameonly")) %>% 
        apply.harmonization("nber-punctuation"
                          , add.spaces = FALSE
                          , del.spaces = FALSE) %>%
        remove.quotes %>% 
        apply.harmonization(c("additional-substitutions"
                            , "nber-standard-name"
                            , "nber-univ")) %>%
        apply.harmonization("additional-regex"
                          , regex = TRUE
                          , add.spaces = FALSE) %>% 
        apply.harmonization(c("additional-removals"
                            , "nber-stem-name")
                          , add.spaces = "right")
}


#' @import magrittr
apply.punctuation <- function(org.names) {
    org.names %>% 
        apply.harmonization("nber-punctuation"
                          , add.spaces = FALSE
                          , del.spaces = FALSE) %>%
        remove.quotes
}
