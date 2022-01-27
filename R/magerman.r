## -------->>  [[file:../harmonizer.src.org::*Characters][Characters:1]]
##' @eval attr(magerman_detect_characters, "description")
##' @param x table
##' @param codes_col_name Same as in `detect_patterns`
##' @inheritDotParams harmonize_options
##' @return coded table
##'
##' @export
magerman_detect_characters <- function(x
           , codes_col_name = "characters_cleaning_candidates"
           , ...) {
        patterns <- c("\\{.+\\}", "propriety coded characters {xxx}"
                    , "\\[0.+\\]", "propriety coded characters [0xxx]"
                    , "\\(.+\\)", "propriety coded characters (xxx)"
                    , "&.+;", "sgml coded characters"
                    , "<.+>", "html coded characters") |>
            matrix(byrow = TRUE, ncol = 2) |>
            data.frame()
        detect_patterns(x
                      , patterns                      
                      , patterns_type = "regex"
                      , codes_col_name = codes_col_name)
    }

attr(magerman_detect_characters, "description") <-
    "Detect candidates for characters that need to be cleaned"
## --------<<  Characters:1 ends here



## -------->>  [[file:../harmonizer.src.org::*magerman.remove.html.codes][magerman.remove.html.codes:1]]
##' Removes html codes
##' @param x table
##' @inheritDotParams harmonize_options
##' @return harmonized table
##'
##' @md
##' @export
magerman_remove_html_codes <- function(x, ...) {
    replace_patterns(x
        , patterns = "<BR>"
        , replacements = " ")
}
## --------<<  magerman.remove.html.codes:1 ends here



## -------->>  [[file:../harmonizer.src.org::*magerman.replace.sgml.characters][magerman.replace.sgml.characters:1]]
##' Replaces sgml characters. Accept both vector and table and return either vector or a table
##' @param x table
##' @inheritDotParams replace_patterns
##' @return Harmonized table
##'
##' @md
##' @export
magerman_replace_sgml_characters <- function(x, ...) {
    replace_patterns(x, magerman_patterns_sgml_characters)
}
## --------<<  magerman.replace.sgml.characters:1 ends here



## -------->>  [[file:../harmonizer.src.org::*magerman.replace.proprietary.characters][magerman.replace.proprietary.characters:1]]
##' Replaces proprietary characters
##' @param x table
##' @inheritDotParams harmonize_options
##' @return Harmonized table
##'
##' @md
##' @export
magerman_replace_proprietary_characters <- function(x, ...) {
    replace_patterns(x, magerman_patterns_proprietary_characters)
}
## --------<<  magerman.replace.proprietary.characters:1 ends here



## -------->>  [[file:../harmonizer.src.org::*magerman.replace.accented.characters][magerman.replace.accented.characters:1]]
##' Replaces accented characters
##'
##' Assumes that all characters are in caps.
##' @param x table
##' @inheritDotParams harmonize_options
##' @return Harmonized table
##'
##' @md
##' @export
magerman_replace_accented_characters <- function(x, ...) {
    replace_patterns(x, magerman_patterns_accented_characters)
}
## --------<<  magerman.replace.accented.characters:1 ends here



## -------->>  [[file:../harmonizer.src.org::*magerman.remove.special.characters][magerman.remove.special.characters:1]]
##' Removes special characters. I.e., everything that is not:
## A-Z; 0-9; “-“; “+”; “’”; “””; “#”; “*”;“@”; “!”; “?”; “/”; “&”; “(“; “)”; “:”; “;”; “,”; “.”; “ “
##' @param x table
##' @inheritDotParams harmonize_options
##' @return Harmonized table
##'
##' @md
##' @export
magerman_remove_special_characters <- function(x, ...) {
    replace_patterns(x
                   , "[^A-Z0-9\\-+'\"#*;@!?/&():;,. ]"
                   , patterns_type = "regex")
}
## --------<<  magerman.remove.special.characters:1 ends here



## -------->>  [[file:../harmonizer.src.org::*magerman.remove.double.spaces][magerman.remove.double.spaces:1]]
##' Removes double spaces
##' @param x table
##' @inheritDotParams replace_patterns
##' @return Harmonized table
##'
##' @export
magerman_remove_double_spaces <- function(x, ...) {
    replace_patterns(x
        , "\\s+"
        , replacements = " "
        , patterns_type = "regex")
}
## --------<<  magerman.remove.double.spaces:1 ends here



## -------->>  [[file:../harmonizer.src.org::*magerman.remove.double.quotation.marks.*][magerman.remove.double.quotation.marks.*:1]]
##' Removes double quotation irregularities
##' @param x table
##' @inheritDotParams harmonize_options
##' @return Harmonized table
##'
##' @export
magerman_remove_double_quotation_marks_irregularities <- function(x, ...) {
    replace_patterns(x
        , patterns = c("^\"\"\\s(.*)\"$", "^\"(.*)\\s\"\"$")
        , replacements = c("\"\"$1\"", "\"$1\"\"")
        , patterns_type = "regex")
}
## --------<<  magerman.remove.double.quotation.marks.*:1 ends here



## -------->>  [[file:../harmonizer.src.org::*magerman.remove.double.quotation.marks.*][magerman.remove.double.quotation.marks.*:3]]
##' Removes double quotation irregularities
##' @param x table
##' @inheritDotParams harmonize_options
##' @return Harmonized table
##'
##' @md
##' @export
magerman_remove_double_quotation_marks_beginning_end <- function(x, ...) {
    replace_patterns(x
        , patterns = "^\"\"((?:(?!\"\").)*)\"\"$"
        , replacements = "$1"
        , patterns_type = "regex")
}
## --------<<  magerman.remove.double.quotation.marks.*:3 ends here



## -------->>  [[file:../harmonizer.src.org::*magerman.remove.non.alphanumeric.*][magerman.remove.non.alphanumeric.*:1]]
##' Removes non alphanumeric characters
##' @param x table
##' @inheritDotParams harmonize_options
##' @return Harmonized table
##'
##' @export
magerman_remove_non_alphanumeric_at_the_beginning <- function(x, ...) {
    replace_patterns(x
        , patterns = "^[^A-Z0-9\"@('#!*/]+"
        , patterns_type = "regex")
}

##' Removes non alphanumeric characters
##' @param x table
##' @inheritDotParams harmonize_options
##' @return Harmonized table
##'
##' @md
##' @export
magerman_remove_non_alphanumeric_at_the_end <- function(x, ...) {
    replace_patterns(x
        , patterns = "[^A-Z0-9.'\")]+$"
        , patterns_type = "regex")
}
## --------<<  magerman.remove.non.alphanumeric.*:1 ends here



## -------->>  [[file:../harmonizer.src.org::*magerman.replace.comma.period.irregularities.*][magerman.replace.comma.period.irregularities.*:1]]
##' Detects comma period irregularities
##' @param x table
##' @param codes_col_name Same as in `detect_patterns`
##' @inheritDotParams harmonize_options
##' @return Harmonized table
##'
##' @export
magerman_detect_comma_period_irregularities <- function(x,
                                                        codes_col_name = "comma.period.irregularities.candidates",
                                                        ...) {
    patterns <- 
        c(",([^\\s])", "Patterns with comma not followed by space"
        , "\\s,", "Patterns with comma preceded by space"
        , "([^A-Za-z0-9])\\.", "Patterns with period not preceded by a letter or digit"
          ) |>
        matrix(byrow = TRUE, ncol = 2) |>
        data.frame()
    detect_patterns(x
                  , patterns
                  , patterns_type = "regex"
                  , codes_col_name = codes_col_name)
}

##' Replaces comma period irregularities
##' @param x table
##' @inheritDotParams harmonize_options
##' @return Harmonized table
##'
##' @export
magerman_replace_comma_period_irregularities_all <- function(x, ...) {
    patterns <- c(
        ",([^\\s])", ", $1",
        "\\s,", ",",
        "([^A-Za-z0-9])\\.", "$1"
    ) |>
        matrix(byrow = TRUE, ncol = 2) |>
        data.frame()
    replace_patterns(x
                   , patterns
                   , patterns_type = "regex"
                     )
}
## --------<<  magerman.replace.comma.period.irregularities.*:1 ends here



## -------->>  [[file:../harmonizer.src.org::*magerman.replace.comma.period.irregularities.*][magerman.replace.comma.period.irregularities.*:3]]
##' Replaces comma period irregularities
##' @param x object
##' @inheritDotParams harmonize_options
##' @return Harmonized table
##'
##' @export
magerman_replace_comma_period_irregularities <- function(x, ...) {
    patterns <- 
        list(magerman_patterns_comma_followed_by_space
           , magerman_patterns_comma_preceded_by_space
           , magerman_patterns_periods) |>
        data.table::rbindlist()
    replace_patterns(x, patterns, patterns_type_col = 3)
}
## --------<<  magerman.replace.comma.period.irregularities.*:3 ends here



## -------->>  [[file:../harmonizer.src.org::*Detect and replace legal forms][Detect and replace legal forms:1]]
##' Detects legal form
##' @param x table
##' @param return_only_codes same as in `detect_patterns`
##' @param patterns_codes same as in `detect_patterns`
##' @param no_match_code same as in `detect_patterns`
##' @param ... 
##' @inheritDotParams harmonize_options
##' @return Harmonized table
##'
##' @md
##' @export
magerman_detect_legal_form_end <- function(x
                                         , return_only_codes = FALSE
                                         , patterns_codes = NULL
                                         , no_match_code = NULL
                                         , codes_update_empty = TRUE
                                         , ...) {
    detect_patterns(x
         , magerman_patterns_legal_form_end
         , patterns_codes_col = 3
         , patterns_codes = patterns_codes
         , patterns_type = "ends"
         , codes_col_name_suffix = "_legal_form"
         , return_only_first_detected_code = TRUE
         , codes_update_empty = codes_update_empty
         , return_only_codes = return_only_codes
         , no_match_code = no_match_code)
}





##' Replaces legal form
##' @param x table
##' @inheritDotParams harmonize_options
##' @return Harmonized table
##'
##' @md
##' @export
magerman_replace_legal_form_end <- function(x, ...) {
    replace_patterns(x
        , patterns = magerman_patterns_legal_form_end
        , patterns_type = "ends")
}

##' Detects legal form
##' @param x table
##' @inheritDotParams harmonize_options
##' @return Harmonzed table
##'
##' @md
##' @export
magerman_detect_legal_form_beginning <- function(x, ...) {
    detect_patterns(x
        , patterns = data.table(pattern = "KABUSHIKI KAISHA"
                              , legal.form = "KAISHA")
        , patterns_type = "begins"
        , return_only_first_detected_code = TRUE
        , codes_col_name_suffix = "_legal_form")
}

##' Replaces legal form
##' @param x table
##' @inheritDotParams harmonize_options
##' @return Harmonized table
##'
##' @md
##' @export
magerman_replace_legal_form_beginning <- function(x, ...) {
    replace_patterns(x
         , patterns = "KABUSHIKI KAISHA"
         , patterns_type = "begins")
}



##' Detects legal form
##' @param x table
##' @inheritDotParams harmonize_options
##' @return Harmonized table
##'
##' @md
##' @export
magerman_detect_legal_form_middle <- function(x, ...) {
    detect_patterns(x
        , patterns = magerman_patterns_legal_form_middle
        , patterns_codes_col = 3
        , patterns_type = "fixed"
        , return_only_first_detected_code = TRUE
        , codes_col_name_suffix = "_legal_form")
}

##' Replaces legal form
##' @param x table
##' @inheritDotParams harmonize_options
##' @return Harmonized table
##'
##' @md
##' @export
magerman_replace_legal_form_middle <- function(x, ...) {
    replace_patterns(x
        , patterns = magerman_patterns_legal_form_middle
        , patterns_type = "fixed")
}
## --------<<  Detect and replace legal forms:1 ends here



## -------->>  [[file:../harmonizer.src.org::*Detect and replace legal forms][Detect and replace legal forms:3]]
##' Detects legal form
##' @param x table
##' @inheritDotParams harmonize_options
##' @return Harmonized table
##'
##' @export
magerman_detect_legal_form <- function(x, ...) {
    x |>
        magerman_detect_legal_form_end() |>
        magerman_detect_legal_form_beginning() |>
        magerman_detect_legal_form_middle()
}

##' Removes legal form
##' @param x table
##' @inheritDotParams harmonize_options
##' @return Harmonized table
##'
##' @export
magerman_remove_legal_form <- function(x, ...) {
    legal_form_end_rows <-
        magerman_detect_legal_form_end(x
                                     , return_only_codes = TRUE
                                     , codes_update_empty = FALSE
                                     , patterns_codes = TRUE
                                     , no_match_code = FALSE)
    x |>
        magerman_replace_legal_form_beginning(rows = !legal_form_end_rows) |>
        magerman_replace_legal_form_middle(rows = !legal_form_end_rows) |>
        magerman_replace_legal_form_end()        
}

##' Removes legal form
##' @param x table
##' @inheritDotParams harmonize_options
##' @return Harmonized table
##' 
##' @export
magerman_remove_legal_form_and_clean <- function(x, ...) {
    x |>
        magerman_remove_legal_form() |>
        replace_patterns(
            patterns = c("[-;:,&]*\\s*$", "^\\s*"),
            patterns_type = "regex"
        )
}
## --------<<  Detect and replace legal forms:3 ends here



## -------->>  [[file:../harmonizer.src.org::*Common Words][Common Words:1]]
##' Removes common words
##' @param x table
##' @inheritDotParams harmonize_options
##' @return Harmonized table
##'
##' @md
##' @export
magerman_remove_common_words_at_the_end <- function(x, ...) {
    replace_patterns(x
        , patterns = magerman_patterns_common_words_at_the_end
        , patterns_type = "ends")
}

##' Removes common words
##' @param x table
##' @inheritDotParams harmonize_options
##' @return Harmonized table
##'
##' @md
##' @export
magerman_remove_common_words_at_the_beginning <- function(x, ...) {
    replace_patterns(x
        , patterns = magerman_patterns_common_words_at_the_beginning
        , patterns_type = "begins")
}



##' Removes common words
##' @param x table
##' @inheritDotParams harmonize_options
##' @return Harmonized table
##'
##' @md
##' @export
magerman_remove_common_words_anywhere <- function(x, ...) {
    replace_patterns(x
        , patterns = magerman_patterns_common_words_anywhere
        , patterns.type = "fixed")
}


##' Removes common words
##' @param x table
##' @inheritDotParams harmonize_options
##' @return Harmonized table
##'
##' @md
##' @export
magerman_remove_common_words <- function(x, ...) {
    x |>
        magerman_remove_common_words_at_the_end() |>
        magerman_remove_common_words_at_the_beginning() |>
        magerman_remove_common_words_anywhere()
}
## --------<<  Common Words:1 ends here



## -------->>  [[file:../harmonizer.src.org::*Spelling Variation][Spelling Variation:1]]
##' Replaces spelling variation
##' @param x table
##' @inheritDotParams harmonize_options
##' @return Harmonized table
##'
##' @md
##' @export
magerman_replace_spelling_variation <- function(x, ...) {
    replace_patterns(x, patterns = magerman_patterns_spelling_variation)
}
## --------<<  Spelling Variation:1 ends here



## -------->>  [[file:../harmonizer.src.org::*Condensing][Condensing:1]]
##' Condenses string
##' @param x table
##' @inheritDotParams harmonize_options
##' @return Harmonized table
##'
##' @md
##' @export
magerman_condense <- function(x, ...) {
    replace_patterns(x
        , patterns = "[^a-zA-Z0-9]+"
        , patterns_type = "regex")
}
## --------<<  Condensing:1 ends here



## -------->>  [[file:../harmonizer.src.org::*Umlaut Harmonization][Umlaut Harmonization:1]]
##' Detects umlauts
##' @param x table
##' @param return_only_codes same as `detect_patterns`
##' @param ... 
##' @inheritDotParams harmonize_options
##' @return Coded table
##'
##' @md
##' @export
magerman_detect_umlaut <- function(x
                                 , return_only_codes = FALSE
                                 , codes_col_name = NULL
                                 , ...) {
    detect_patterns(x
                  , patterns = magerman_patterns_umlaut
                  , patterns_type = "fixed"
                  , patterns_codes = TRUE
                  , no_match_code = FALSE
                  , codes_col_name_suffix = "_has_umlaut"
                  , codes_col_name = codes_col_name
                  , return_only_first_detected_code = TRUE
                  , return_only_codes = return_only_codes)
}

##' Replaces Umlauts
##' @param x Data
##' @param has_umlaut_col Column with logical values indicating weather a corresponding string has an umlaut. Default is NULL so it detects is automatically first
##' @param drop_has_umlaut_col Whether to drop `has_umlaut_col`. Default is FALSE
##' @param replace_accented_characters Whether to replace accented characters first. Default is FALSE
##' @inheritDotParams harmonize_options
##' @return Harmonized table
##'
##' @md
##' @export
magerman_replace_umlaut <- function(x,
                                    has_umlaut_col = NULL,
                                    drop_has_umlaut_col = TRUE,
                                    replace_accented_characters = FALSE,
                                    ...) {
    ## get x vector...
    x_vector <- get_target(x)
    ## identify names with umlauts
    if (!is.null(has_umlaut_col) & !is.atomic(x)) {
        has_umlaut <-
            get_target(x, col = has_umlaut_col, output = "replace_col")
        checkmate::assert_logical(has_umlaut)
        ## drop has_umlaut_col
        if (isTRUE(drop_has_umlaut_col)) x[[has_umlaut_col]] <- NULL
    } else {
        replace_accented_characters <- TRUE
        has_umlaut <- 
            do.call(magerman_detect_umlaut
                  , c(list(x_vector)
                    , list(return_only_codes = TRUE)
                    , formals("harmonize_options"))) |>
            as.logical()
    }
    ## replace accented characters
    if (replace_accented_characters) {
        ## do magerman_replace_accented_characters with defauls
        x_vector <-
            do.call(magerman_replace_accented_characters
                  , c(list(x_vector), formals("harmonize_options")))
    }
    ## check if there are at least some umlauts
    if (any(has_umlaut)) {
        ## transform umlaut
        ## first "AE", "OE", "UE" -> "A", "O", "U"
        x_harmonized <- 
            do.call(replace_patterns
                  , c(list(x_vector)
                    , formals("harmonize_options")
                    , list(patterns = magerman_patterns_umlaut
                         , patterns_col = 3
                         , patterns_replacements_col = 2)))
        ## then "A", "O", "U" -> "AE", "OE", "UE"
        x_harmonized <- 
            do.call(replace_patterns
                  , c(list(x_harmonized)
                    , formals("harmonize_options")
                    , list(patterns = magerman_patterns_umlaut
                          , patterns_col = 2
                         , patterns_replacements_col = 3)))
        ## check which one match original umlaut
        x_harmonized_keep <-
            x_harmonized %in%
            x_harmonized[sapply(has_umlaut, isTRUE)]
        ## if does not match umlaut replace with original
        x_harmonized[!x_harmonized_keep] <-
            x_vector[!x_harmonized_keep]
        ## return table
        return(inset_target(x_harmonized, x))
    } else {
        return(inset_target(x_vector, x))
    }
}
## --------<<  Umlaut Harmonization:1 ends here



## -------->>  [[file:../harmonizer.src.org::*Combined Magerman Procedures][Combined Magerman Procedures:1]]
##' Harmonizes strings using exact procedures described in Magerman et al. 2009.
##' @param x table or vector
##' @param detect_legal_form Whether to detect legal forms. Default is FALSE
##' @param append_copy_before_common_words_removal Whether to save harmonized column before `common.words.removal` procedure. Default is FALSE
##' @inheritDotParams harmonize
##' @return Harmonized table
##'
##' @references Magerman et al., 2006 - Data Production Methods for Harmonized Patent Statistics: Patentee Name Harmonization
##'
##' @md
##' @export
harmonize_magerman <- function(x
                             , detect_legal_form = FALSE
                             , append_copy_before_common_words_removal = FALSE
                             , ...) {
    magerman_procedures <-
        harmonize_make_procedures_list(magerman_procedures_table)
    ## do some tweaks on magerman_procedures
    if (!detect_legal_form) {
        is_magerman_detect_legal_form <- 
            sapply(magerman_procedures
                 , \(p) p[[1]] == "magerman_detect_legal_form")
        magerman_procedures <-
            magerman_procedures[!is_magerman_detect_legal_form]
    }
    if (append_copy_before_common_words_removal) {
        which_is_magerman_remove_legal_form_and_clean <-
            sapply(magerman_procedures
                 , \(p) p[[1]] == "magerman_remove_legal_form_and_clean") |>
            which()
        magerman_procedures[[which_is_magerman_remove_legal_form_and_clean]] <-
            c(magerman_procedures[[which_is_magerman_remove_legal_form_and_clean]]
            , list(append_copy = TRUE
                 , append_copy_name_format = "{col_name}_before_common_words_removal"))
    }
    harmonize(x, magerman_procedures, ...)
}
## --------<<  Combined Magerman Procedures:1 ends here


