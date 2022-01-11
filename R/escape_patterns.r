## -------->>  [[file:../harmonizer.src.org::*harmonize_escape_regex][harmonize_escape_regex:1]]
##' Escapes special for regex characters
##' @param string character vector
##' @return character vector with all special to regex characters escaped
##'
##' @export
escape_regex <- function(string) stringi::stri_replace_all_regex(string, "(\\W)", "\\\\$1")

##' Escapes special for different types of pattern
##' @param string character vector (patterns)
##' @param type How the characters in patterns should be 'escaped' for
##'     proper matching. The options are:
##' @param return_regex Always return regex. Default is
##'     true. Otherwise return unescaped string for 'fixed' and
##'     'exact' types and just trimmed string for "trim_exact"
##' @param return_docs Return character vector of documentation for
##'     each type (for using with roxygen2 documentations in many
##'     places)
##' @param return_choices Return character vector of available escape
##'     types
##' @return character vector with all special to regex characters
##'     escaped
##' @eval escape_regex_for_type(return_docs = TRUE)
##' @md
escape_regex_for_type <- function(string
                                , type
                                , return_regex = TRUE
                                , return_docs = FALSE
                                , return_choices = FALSE) {
    types_specs <- list(
        list(type = "fixed"
           , docs = "Match pattern string as it is within the target vector"
           , func = \(string, return_regex) {
               if(return_regex) escape_regex(string)
               else string
           })
      , list(type = "begins"
           , docs = "Match pattern string as it is in the beggining of the target vector"
           , func = \(string, return_regex) {
               paste0("^", escape_regex(string))
           })
      , list(type = "trim_begins"
           , docs = "Match pattern string as it is in the beginning of the target vector ignoring preceding white-spaces"
           , func = \(string, return_regex){
               paste0("^\\s*", escape_regex(string))
           })
      , list(type = "ends"
           , docs = "Match pattern string as it is in the end of the target vector"
           , func = \(string, return_regex) {
               paste0(escape_regex(string), "$")
           })
      , list(type = "trim_ends"
           , docs = "Match pattern string as it is in the end of the target vector ignoring leading white-spaces"
           , func = \(string, return_regex) {
               paste0(escape_regex(string), "\\s*$")
           })
      , list(type = "exact"
           , docs = "Match pattern string exactly (i.e., match equal strings)"
           , func = \(string, return_regex) {
               if(return_regex) paste0("^", escape_regex(string), "$")
               else string
           })
      , list(type = "trim_exact"
           , docs = "Match pattern string exactly (i.e., match equal strings) ignoring surrounding white-spaces"
           , func = \(string, return_regex) {
               if(return_regex) paste0("^\\s*", escape_regex(string), "\\s*$")
               else stringi::stri_trim_both(string)
           })
      , list(type = "regex"
           , docs = "Match regex pattern"
           , func = \(string, return_regex) {
               string
           }))
    if(return_docs) {
        return(sapply(types_specs, \(x) paste("* ", x$type, " - ", x$docs)))
    }
    if(return_choices) {
        return(sapply(types_specs, `[[`, "type"))
    }
    types_funcs <- lapply(types_specs, `[[`, "func")
    names(types_funcs) <- lapply(types_specs, `[[`, "type")
    ## this will return the right func and call it
    do.call(switch, c(list(EXPR = type), types_funcs))(string, return_regex)
}

##' Escapes special for regex characters conditionally
##' @param patterns character vector
##' @param return_regex Always return regex. Default is
##'     true. Otherwise return unescaped string for 'fixed' and
##'     'exact' types and just trimmed string for "trim_exact"
##' @param types character vector of the same length as `strings` with
##'     instructions whether and how to to escape regex characters. Options are:
##' @eval escape_regex_for_type(return_docs = TRUE)
##' @return string with all special to regex characters escaped
##'
##' @import stringr
escape_regex_for_types <- function(patterns, types, return_regex = FALSE) {
    if(length(types) == 1 || length(unique(types)) == 1) {
        type <- types[1]
        checkmate::assert_choice(type
                               , choices = escape_regex_for_type(return_choices = TRUE)
                               , fmatch = TRUE)
        escape_regex_for_type(patterns, type, return_regex = return_regex)
    } else {
        checkmate::assert_subset(types
                               , choices = escape_regex_for_type(return_choices = TRUE)
                               , fmatch = TRUE)
        mapply(\(pattern, type) escape_regex_for_type(pattern, type)
             , patterns , types
             , SIMPLIFY = TRUE)
    }
}
## --------<<  harmonize_escape_regex:1 ends here


