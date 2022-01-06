## -------->>  [[file:../harmonizer.src.org::*harmonize_replace][harmonize_replace:1]]
## a wrapper for stri_replace to use in Reduce
stri_replace_do <- function(str, arg_list) {
    do.call(stri_replace, c(list(str), arg_list))
}


#' A wrapper for string replacement and cbinding some columns.
#'
#' Optionally matches only at the beginning or at the end of the string.
#' 
#' @param x Vector or table to harmonize.
#' @param patterns Accepts both vector or table. If patterns is a table can also include replacements column.
#' @param patterns_col If patterns is not a vector which column to use. Default is 1.
#' @param patterns_mode Mode of matching. Could be one of c("all", "first", "last"). The default is "all" (it is 2x faster than "first" and "last" because of handy stri_replace_all_* functions). Also possible to pass a vector (same length as patterns)
#' @param patterns_mode_col  Column in patterns table with the mode of matching
#' @param patterns_type Kind of pattern. Default is "fixed" (calling code{\link[stringi]{stri_replace_all_fixed}}). Other options are "begins", "ends" - which means that it should only match fixed pattern at the beginning of the string or at the and. Another possible value is "regex" (calling code{\link[stringi]{stri_replace_all_regex}})
#' @param patterns_type_col Column with the type of pattern in case when patterns should have different types
#' @param patterns_replacements_col If patterns is not a vector and includes replacements which column to use for replacements. Default is 2.
#' @param replacements If patterns does not have column with replacements provide it here.
#' @inheritDotParams harmonize_options
#'
#' @return If nothing was indicated to cbind to results then it returns harmonized vector. If something is needs to be cbind then it returns data.table
#' @import stringi stringr magrittr
#' 
#' @export
harmonize_replace <- function(x
                            , patterns
                            , patterns_col = 1
                            , patterns_mode = "all"
                            , patterns_mode_col = NULL
                            , patterns_type = "fixed"
                            , patterns_type_col = NULL
                            , patterns_replacements_col = 2
                            , replacements = ifelse(is.atomic(patterns), "", NULL)
                            , ...) {
    ## check arguments

    ## get vectors
    x.vector <- get_target(x)
    patterns.vector <- get_vector(patterns, patterns_col)
    ## types
    types.vector <- get_vector(patterns, patterns_type_col
                             , fallback_value = patterns_type
                             , choices = c("fixed"
                                              , "begins"
                                              , "begins.trimmed"
                                              , "ends"
                                              , "ends.trimmed"
                                              , "regex"
                                              , "exact"
                                              , "exact.trimmed"))
    ## modes
    modes.vector <- get_vector(patterns, patterns_mode_col
                             , fallback_value = patterns_mode
                             , choices = c("all", "first", "last"))
    ## replacements
    replacements.vector <- get_vector(patterns, patterns_replacements_col
                                    , fallback_value = replacements
                                    , fallback_value_ignored_if_col = FALSE)
    ## do replace and return
    ## make patterns.vector excaped according to types.vector
      patterns.vector <- harmonize_escape_types(patterns.vector, types.vector)
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
          Reduce(stri_replace_do
               , Map(list
                   , fixed = patterns.vector
                   , replacement = replacements.vector
                   , mode = modes.vector)
               , init = x.vector)
      } else {
          Reduce(stri_replace_do
               , Map(list
                   , regex = patterns.vector
                   , replacement = replacements.vector
                   , mode = modes.vector)
               , init = x.vector)
      }
}
## --------<<  harmonize_replace:1 ends here


