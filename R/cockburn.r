## -------->>  [[file:../harmonizer.src.org::*cockburn_combabbrev][cockburn_combabbrev:1]]
##' Collapses single character sequences
##'
##' @param x Object (table or vector)
##' @param wrap_in_spaces Whether to wrap strings in spaces before processing because the algorithm assumes assumes that each string begins and ends with space. Default is TRUE.
##' @inheritDotParams harmonize_options
##' @return Harmonized table
##' 
##' @md 
##' @export 
cockburn_combabbrev <- function(x
                              , wrap_in_spaces = TRUE
                              , ...) {
    x_vector <- get_target(x) 
    ## wrap in spaces
    if (wrap_in_spaces) {
        x_vector <- paste0(" ", x_vector, " ")
    }
    ## collapse
    sapply(x_vector, \(org_name) {
        reg  <- gregexpr("(?=\\s\\w(\\s+)\\w\\s)", org_name, perl = TRUE)
        ## check if there are matches
        if(reg[[1]][1] != -1) {
            char <- strsplit(org_name, "", fixed = TRUE) |> unlist()
            pos <- mapply(function(from, length.out) seq(from, length.out = length.out)
                        , from = attr(reg[[1]],"capture.start")
                        , length.out = attr(reg[[1]],"capture.length")
                        , SIMPLIFY = FALSE) |> unlist()
            char[pos] <- ""
            char |> paste(collapse = "")
        } else {
            org_name
        }
    }, USE.NAMES = FALSE) |>
        inset_target(x)
}
## --------<<  cockburn_combabbrev:1 ends here



## -------->>  [[file:../harmonizer.src.org::*Derwent][Derwent:1]]
##' @eval attr(cockburn_replace_derwent, "@title")
##' @description It is a version from Cockburn, I. M., A. Agrawal,
##'     J. Bessen, J. H. S. Graham, B. H. Hall, and M. MacGarvie
##'     (2009), The NBER Patent Citations Datafile Update. It differs
##'     from original dervert standartization
##' @inherit replace_patterns params return
##' @inheritDotParams harmonize_options
##' @return Harmonized table
##' @family magerman
##' @seealso replace_patterns
##'
##' @md
##' @export
cockburn_replace_derwent <- make_alias(replace_patterns
                                     , patterns = cockburn_patterns_derwent
                                     , patterns_mode = "first")

attr(cockburn_replace_derwent, "@title") <-
    "Performs Derwent standardization of organizational names"
## --------<<  Derwent:1 ends here



## -------->>  [[file:../harmonizer.src.org::*Compustat][Compustat:1]]
##' @eval attr(cockburn_replace_compustat, "@title")
##' @inherit replace_patterns params return
##' @inheritDotParams harmonize_options
##' @return Harmonized table
##' @family magerman
##' @seealso replace_patterns
##'
##' @md
##' @export
cockburn_replace_compustat <- make_alias(replace_patterns
                                       , patterns = cockburn_patterns_compustat)

attr(cockburn_replace_compustat, "@title") <-
    "COMPUSTAT specific standardization for organizational names"



##' @eval attr(cockburn_replace_compustat_names, "@title")
##' @inherit replace_patterns params return
##' @inheritDotParams harmonize_options
##' @return Harmonized table
##' @family magerman
##' @seealso replace_patterns
##'
##' @md
##' @export
cockburn_replace_compustat_names <- make_alias(replace_patterns
                                             , patterns = cockburn_patterns_compustat_names
                                             , patterns_type = "trim_exact")

attr(cockburn_replace_compustat_names, "@title") <-
    "COMPUSTAT specific standardization for organizational names. Full name replacements."
## --------<<  Compustat:1 ends here



## -------->>  [[file:../harmonizer.src.org::*Identify Entity Type][Identify Entity Type:1]]
##' Identifies Entity Type
##'
##' @param x vector or table
##' @param verbose For debuging. If set will message which procedures were done.
##' @inheritDotParams harmonize.detect
##' @return Harmonized table
##' 
##' @md 
##' @export 
cockburn_detect_type <- function(x
                               , verbose = FALSE
                               , ...) {
    do_verbosely <- \(x, fun) {
        fun_name <- deparse(substitute(fun))
        if(verbose) message("- ", fun_name)
        x <- do.call(fun, list(x))
        return(x)
    }
    x |> 
        do_verbosely(cockburn_detect_corp) |>
        do_verbosely(cockburn_detect_indiv) |>
        do_verbosely(cockburn_detect_govt) |>
        do_verbosely(cockburn_detect_univ) |>
        do_verbosely(cockburn_detect_inst) |>
        do_verbosely(cockburn_detect_inst_conds) |>
        do_verbosely(cockburn_detect_inst_german) |>
        do_verbosely(cockburn_detect_hosp)
}


##' Cleanup Entity Type
##'
##' @param x vector or table
##' @inheritDotParams replace_patterns
##' @return Harmonized table
##' 
##' @md 
##' @export 
cockburn_replace_type <- function(x, ...) {
    x |> 
        cockburn_replace_govt() |> 
        cockburn_replace_univ()
}
## --------<<  Identify Entity Type:1 ends here



## -------->>  [[file:../harmonizer.src.org::*Firms (Corporates)][Firms (Corporates):1]]
##' @eval attr(cockburn_detect_corp, "@title")
##' @description From non_corporates.do file. Source - https://sites.google.com/site/patentdataproject/Home/posts/namestandardizationroutinesuploaded
##' @inherit detect_patterns params return
##' @inheritDotParams harmonize_options
##' @return Harmonized table
##' @family magerman
##' @seealso detect_patterns
##'
##' @md
##' @export
cockburn_detect_corp <- make_alias(detect_patterns
                                 , patterns = cockburn_patterns_corp
                                 , output_codes_col_name = "entity_type"
                                 , merge_existing_codes = "append_to_existing"
                                 , patterns_codes = "firm"
                                 , return_only_first_detected_code = TRUE)

attr(cockburn_detect_corp, "@title") <-
    "Detect Corporates (code - 'firm')"
## --------<<  Firms (Corporates):1 ends here



## -------->>  [[file:../harmonizer.src.org::*Individuals][Individuals:1]]
##' @eval attr(cockburn_detect_indiv, "@title")
##' @description From non_corporates.do file. Source -
##'     https://sites.google.com/site/patentdataproject/Home/posts/namestandardizationroutinesuploaded
##' @inherit detect_patterns params return
##' @inheritDotParams harmonize_options
##' @return Harmonized table
##' @family magerman
##' @seealso detect_patterns
##'
##' @md
##' @export
cockburn_detect_indiv <- make_alias(detect_patterns
                                  , patterns = cockburn_patterns_indiv
                                  , patterns_codes = "indiv"
                                  , output_codes_col_name = "entity_type"
                                  , merge_existing_codes = "append_to_existing"
                                  , return_only_first_detected_code = TRUE)

attr(cockburn_detect_indiv, "@title") <-
    "Detect Individuals (Non-Corporates group)"
## --------<<  Individuals:1 ends here



## -------->>  [[file:../harmonizer.src.org::*Government][Government:1]]
##' @eval attr(cockburn_detect_govt, "@title")
##' @description From non_corporates.do file. Source - https://sites.google.com/site/patentdataproject/Home/posts/namestandardizationroutinesuploaded
##' @inherit detect_patterns params return
##' @inheritDotParams harmonize_options
##' @return Harmonized table
##' @family magerman
##' @seealso detect_patterns
##'
##' @md
##' @export
cockburn_detect_govt <- make_alias(detect_patterns
                                 , patterns = cockburn_patterns_govt
                                 , patterns_codes = "govt"
                                 , output_codes_col_name = "entity_type"
                                 , merge_existing_codes = "append_to_existing"
                                 , return_only_first_detected_code = TRUE)

attr(cockburn_detect_govt, "@title") <-
    "Detect Goverment Organizations (Non-Corporates group)"




##' @eval attr(cockburn_replace_govt, "@title")
##' @description From non_corporates.do file. Source - https://sites.google.com/site/patentdataproject/Home/posts/namestandardizationroutinesuploaded
##' @inherit replace_patterns params return
##' @inheritDotParams harmonize_options
##' @return Harmonized table
##' @family magerman
##' @seealso replace_patterns
##'
##' @md
##' @export
cockburn_replace_govt <- make_alias(replace_patterns
                                  , patterns = cockburn_patterns_govt_cleanup)

attr(cockburn_replace_govt, "@title") <-
    "Cleanup Goverment Organizations (Non-Corporates group)"
## --------<<  Government:1 ends here



## -------->>  [[file:../harmonizer.src.org::*Universities][Universities:1]]
##' @eval attr(cockburn_detect_univ, "@title")
##' @description From non_corporates.do file. Source - https://sites.google.com/site/patentdataproject/Home/posts/namestandardizationroutinesuploaded
##' @inherit detect_patterns params return
##' @inheritDotParams harmonize_options
##' @return Harmonized table
##' @family magerman
##' @seealso detect_patterns
##'
##' @md
##' @export
cockburn_detect_univ <- make_alias(detect_patterns
                                 , patterns = cockburn_patterns_univ
                                 , patterns_codes = "univ"
                                 , output_codes_col_name = "entity_type"
                                 , merge_existing_codes = "append_to_existing"
                                 , return_only_first_detected_code = TRUE)

attr(cockburn_detect_univ, "@title") <-
    "Detect Universities (Non-Corporates group)"







##' @eval attr(cockburn_replace_univ, "@title")
##' @description From non_corporates.do file. Source - https://sites.google.com/site/patentdataproject/Home/posts/namestandardizationroutinesuploaded
##' @inherit replace_patterns params return
##' @inheritDotParams harmonize_options
##' @return Harmonized table
##' @family magerman
##' @seealso replace_patterns
##'
##' @md
##' @export
cockburn_replace_univ <- make_alias(replace_patterns
                                  , patterns = cockburn_patterns_univ_cleanup)

attr(cockburn_replace_univ, "@title") <-
    "Cleanup Universities (Non-Corporates group)"
## --------<<  Universities:1 ends here



## -------->>  [[file:../harmonizer.src.org::*Non-profit institutes][Non-profit institutes:1]]
##' @eval attr(cockburn_detect_inst, "@title")
##' @description From non_corporates.do file. Source - https://sites.google.com/site/patentdataproject/Home/posts/namestandardizationroutinesuploaded
##' @inherit detect_patterns params return
##' @inheritDotParams harmonize_options
##' @return Harmonized table
##' @family magerman
##' @seealso detect_patterns
##'
##' @md
##' @export
cockburn_detect_inst <- make_alias(detect_patterns
                                 , patterns = cockburn_patterns_inst
                                 , patterns_codes = "inst"
                                 , output_codes_col_name = "entity_type"
                                 , merge_existing_codes = "append_to_existing"
                                 , return_only_first_detected_code = TRUE)

attr(cockburn_detect_inst, "@title") <-
    "Detect Non-profit Institutes (Non-Corporates group)"
## --------<<  Non-profit institutes:1 ends here



## -------->>  [[file:../harmonizer.src.org::*Complex conditions][Complex conditions:1]]
##' @eval attr(cockburn_detect_inst_conds_1, "@title")
##' @description STATA equivalent: replace asstype = "inst" if strpos(standard_name," COUNCIL OF ")>0 & strpos(standard_name," RES ")>0
##' @inherit detect_patterns params return
##' @inheritDotParams harmonize_options
##' @return Harmonized table
##' @family magerman
##' @seealso detect_patterns
##'
##' @md
##' @export
cockburn_detect_inst_conds_1 <- make_alias(detect_patterns
                                         , patterns = " COUNCIL OF .* RES | RES .* COUNCIL OF "
                                         , patterns_type = "regex"
                                         , patterns_codes = "inst"
                                         , output_codes_col_name = "entity_type"
                                         , merge_existing_codes = "append_to_existing"
                                         , return_only_first_detected_code = TRUE)

attr(cockburn_detect_inst_conds_1, "@title") <-
    "Detects Non-profit institutes with special conditions"



##' Detects Non-profit institutes with special conditions
##'
##' STATA equivalent
##' replace asstype = "inst" if strpos(standard_name," FOUND ")~=0 & asstype~="univ"
##' assume a bug: " FOUND ")~=0 -> " FOUND ")>0
##' replace asstype = "inst" if strpos(standard_name," INST ")>0 & asstype~="univ"
##' 
##' @param x table. Expected that x has a column with codes for universities
##' @param output_codes_col_name column with codes for universities ("univ"). Default is last column of x
##' @param merge_existing_codes same as in [detect_patterns()]
##' @inheritDotParams harmonize_options
##' @return Harmonized table
##' 
##' @md
##' @export
cockburn_detect_inst_conds_2 <- function(x
                                       , output_codes_col_name = "entity_type"
                                       , merge_existing_codes = "append_to_existing"
                                       , ...) {

    codes <- get_target(x
                      , col = output_codes_col_name
                      , return_null_for_new_col = TRUE)
    conds <-
        lapply(codes, `%in%`, "univ") |>
        sapply(any, na.rm = TRUE)
    conds <- if(length(conds) == 0) NULL else !conds
    x_vec <- get_target(x)
    .x <- data.table(x_vec, codes)
    names(.x) <- c("x", output_codes_col_name)
    coded <- detect_patterns(x = .x
                           , patterns = c(" FOUND "
                                         , " INST ")
                           , col = 1
                           , output_placement = "replace_col"
                           , rows = conds
                           , output_codes_col_name = output_codes_col_name
                           , patterns_codes = "inst"
                           , merge_existing_codes = merge_existing_codes
                           , return_only_first_detected_code = TRUE
                           , return_only_codes = TRUE)
    inset_target(coded, x, col = output_codes_col_name, output_placement = "replace_col")
}




##' Detects Non-profit institutes with special conditions
##'
##' @param x table. Expected that x has a column with codes for universities
##' @param merge_existing_codes same as in [detect_patterns()]
##' @param output_codes_col_name column with codes for universities ("univ"). Default is last column of x
##' @inheritDotParams harmonize_options
##' @return Harmonized table
##' 
##' @md
##' @export
cockburn_detect_inst_conds <- function(x
                                     , merge_existing_codes = "append_to_existing"
                                     , output_codes_col_name = "entity_type"
                                     , ...) {
  x |> 
      cockburn_detect_inst_conds_1(merge_existing_codes = merge_existing_codes
                                 , output_codes_col_name = output_codes_col_name) |>
      cockburn_detect_inst_conds_2(output_codes_col_name = output_codes_col_name
                                 , merge_existing_codes = merge_existing_codes)
}
## --------<<  Complex conditions:1 ends here



## -------->>  [[file:../harmonizer.src.org::*German Non-profit institutes][German Non-profit institutes:1]]
##' @eval attr(cockburn_detect_inst_german, "@title")
##' @description "EINGETRAGENER VEREIN. NON PROFIT SOCIETY/ASSOCIATION."
##' @param x table
##' @param output_codes_col_name same as in [detect_patterns()]
##' @param merge_existing_codes same as in [detect_patterns()]
##' @inheritDotParams harmonize_options
##' @return Harmonized table
##' 
##' @md 
##' @export 
cockburn_detect_inst_german <- function(x
                               , output_codes_col_name = "entity_type"
                               , merge_existing_codes = "append_to_existing"
                               , ...) {
    rows <- get_col_and_rows()$rows
    conds <- detect_patterns(x
                           , patterns = c(" UNIV "
                                        , " GMBH "
                                        , " KGAA "
                                        , " KG "
                                        , " AG "
                                        , " EG "
                                        , " OHG ")
                           , patterns_codes = TRUE
                           , no_match_code = FALSE
                           , return_only_first_detected_code = TRUE
                           , return_only_codes = TRUE)
    detect_patterns(x, patterns = c(" STIFTUNG "
                                  , " EINGETRAGENER VEREIN ")
                  , output_codes_col_name = output_codes_col_name
                  , merge_existing_codes = merge_existing_codes
                  , rows = and_rows(rows, conds, x)
                  , patterns_codes = "inst"
                  , return_only_first_detected_code = TRUE)
}

attr(cockburn_detect_inst_german, "@title") <- "Detects German Non-profit institutes"
## --------<<  German Non-profit institutes:1 ends here



## -------->>  [[file:../harmonizer.src.org::*Hospitals][Hospitals:1]]
##' @eval attr(cockburn_detect_hosp, "@title")
##' @description From non_corporates.do file. Source - https://sites.google.com/site/patentdataproject/Home/posts/namestandardizationroutinesuploaded
##' @inherit detect_patterns params return
##' @inheritDotParams harmonize_options
##' @return Harmonized table
##' @family magerman
##' @seealso detect_patterns
##'
##' @md
##' @export
cockburn_detect_hosp <- make_alias(detect_patterns
                                 , patterns = cockburn_patterns_hosp
                                 , patterns_codes = "hosp"
                                 , return_only_first_detected_code = TRUE
                                 , output_codes_col_name = "entity_type"
                                 , merge_existing_codes = "append_to_existing")

attr(cockburn_detect_hosp, "@title") <-
    "Detect Hospitals (Non-Corporates group)"
## --------<<  Hospitals:1 ends here



## -------->>  [[file:../harmonizer.src.org::*Punctuation][Punctuation:1]]
##' Removes punctuation and standardise some symbols. 
##'
##' @param x object
##' @inheritDotParams harmonize_options
##' @return Harmonized table
##' 
##' @md
##' @export 
cockburn_replace_punctuation <- function(x
                                         , ...) {
  x |>
      replace_patterns(patterns = cockburn_replace_punctuation_and) |>
      replace_patterns(patterns = cockburn_replace_punctuation_the
                     , patterns_type_col = 3) |>
      ## I swapted patstat with amadeus otherwise Ã²Ã¢ÃªÃ®Ã© will not become oaeie
      replace_patterns(patterns = cockburn_replace_punctuation_patstat) |> 
      replace_patterns(patterns = cockburn_replace_punctuation_amadeus) |>
      replace_patterns(patterns = cockburn_replace_punctuation_char)
}
## --------<<  Punctuation:1 ends here



## -------->>  [[file:../harmonizer.src.org::*Standard Name][Standard Name:1]]
##' Create standard name
##'
##' @param x object
##' @inheritDotParams replace_patterns
##' @return Harmonized table
##' 
##' @md
##' @export 
cockburn_replace_standard_names <- function(x
                                            , ...) {
  x |>
    cockburn_replace_derwent() |> 
    replace_patterns(patterns = cockburn_patterns_standard_names_additional) |> 
    replace_patterns(patterns = cockburn_patterns_standard_names_country_specific)
}
## --------<<  Standard Name:1 ends here



## -------->>  [[file:../harmonizer.src.org::*Stem Name][Stem Name:1]]
##' @eval attr(cockburn_remove_standard_names, "@title")
##' @inherit replace_patterns params return
##' @inheritDotParams harmonize_options
##' @return Harmonized table
##' @family magerman
##' @seealso replace_patterns
##'
##' @md
##' @export
cockburn_remove_standard_names <- make_alias(replace_patterns
                                           , patterns = cockburn_patterns_stem_name
                                           , replacements = " ")

attr(cockburn_remove_standard_names, "@title") <-
    "Creates so called stem name (a name with all legal entity identifiers removed)"
## --------<<  Stem Name:1 ends here



## -------->>  [[file:../harmonizer.src.org::*USPTO special][USPTO special:1]]
##' @eval attr(cockburn_remove_uspto, "@title")
##' @inherit replace_patterns params return
##' @inheritDotParams harmonize_options
##' @return Harmonized table
##' @family magerman
##' @seealso replace_patterns
##'
##' @md
##' @export
cockburn_remove_uspto <- make_alias(replace_patterns
                                  , patterns = cockburn_patterns_uspto)

attr(cockburn_remove_uspto, "@title") <-
    "Removes special USPTO codes."




##' @eval attr(cockburn_detect_uspto, "@title")
##' @inherit detect_patterns params return
##' @inheritDotParams harmonize_options
##' @return Harmonized table
##' @family magerman
##' @seealso detect_patterns
##'
##' @md
##' @export
cockburn_detect_uspto <- make_alias(detect_patterns
                                  , patterns = ";"
                                  , patterns_codes = "indiv"
                                  , output_codes_col_name = "entity_type"
                                  , return_only_first_detected_code = TRUE)

attr(cockburn_detect_uspto, "@title") <-
    "Special USPTO codes. Codes as 'indiv'"
## --------<<  USPTO special:1 ends here



## -------->>  [[file:../harmonizer.src.org::*Combined Cockburn Procedures][Combined Cockburn Procedures:1]]
##' Harmonizes strings using exact procedures described in Cockburn, et al. (2009)
##' @param x table or vector
##' @param cockburn_procedures list of procedures to pass to `harmonize` function. Default is `cockburn_procedures.list`
##' @param detect_legal_form Whether to detect legal forms. Default is FALSE
##' @param return_x_before_common_words_removal Whether to save harmonized column before `common.words.removal` procedure. Default is FALSE
##' @inheritDotParams harmonize
##' @return Harmonized table
##'
##' @references Cockburn, et al. (2009)
##' 
##' @md 
##' @export 
harmonize_cockburn <- function(x
                             , cockburn_procedures = cockburn_procedures_table
                             , detect_legal_form = FALSE
                             , return_x_before_common_words_removal = FALSE
                             , ... ) {
    if(is.data.frame(cockburn_procedures)) {
        cockburn_procedures <- harmonize_make_procedures_list(cockburn_procedures)
    }
  ## do some tweaks on cockburn_procedures
  if(!detect_legal_form) {
      cockburn_procedures <-
          cockburn_procedures[
              !(sapply(cockburn_procedures, `[[`, 1) %in% 
                c("cockburn_detect_type", "cockburn_detect_uspto"))
          ]

  }
    if(return_x_before_common_words_removal) {
      cockburn_procedures[[
          which(sapply(cockburn_procedures, `[[` , 1) %in% "cockburn_combabbrev")
      ]] <- list("cockburn_combabbrev", append_output_copy = TRUE)
  }
  harmonize(x, cockburn_procedures, ...)
}
## --------<<  Combined Cockburn Procedures:1 ends here


