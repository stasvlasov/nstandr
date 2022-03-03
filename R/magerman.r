## -------->>  [[file:../harmonizer.src.org::*Characters][Characters:1]]
##' @eval attr(magerman_detect_characters, "@title")
##' @inherit detect_patterns params return
##' @inheritDotParams harmonize_options
##' @return Harmonized table
##' @family magerman
##' @seealso detect_patterns
##'
##' @md
##' @export
magerman_detect_characters <- make_alias(detect_patterns
                                       , patterns =
                                             c("\\{.+\\}", "propriety coded characters {xxx}"
                                             , "\\[0.+\\]", "propriety coded characters [0xxx]"
                                             , "\\(.+\\)", "propriety coded characters (xxx)"
                                             , "&.+;", "sgml coded characters"
                                             , "<.+>", "html coded characters") |>
                                             matrix(byrow = TRUE, ncol = 2) |>
                                             data.frame()
                                       , patterns_type = "regex"
                                       , output_codes_col_name = "characters_cleaning_candidates")

attr(magerman_detect_characters, "@title") <-
    "Detect candidates for characters that need to be cleaned"
## --------<<  Characters:1 ends here



## -------->>  [[file:../harmonizer.src.org::*magerman.remove.html.codes][magerman.remove.html.codes:1]]
##' @eval attr(magerman_remove_html_codes, "@title")
##' @inherit replace_patterns params return
##' @inheritDotParams harmonize_options
##' @return Harmonized table
##' @family magerman
##' @seealso replace_patterns
##'
##' @md
##' @export
magerman_remove_html_codes <- make_alias(replace_patterns
                                       , patterns = "<BR>"
                                       , replacements = " ")


attr(magerman_remove_html_codes, "@title") <-
    "Removes html codes"
## --------<<  magerman.remove.html.codes:1 ends here



## -------->>  [[file:../harmonizer.src.org::*magerman.replace.sgml.characters][magerman.replace.sgml.characters:1]]
##' @eval attr(magerman_replace_sgml_characters, "@title")
##' @inherit replace_patterns params return
##' @inheritDotParams harmonize_options
##' @return Harmonized table
##' @family magerman
##' @seealso replace_patterns
##'
##' @md
##' @export
magerman_replace_sgml_characters <- make_alias(replace_patterns
                                             , patterns = magerman_patterns_sgml_characters)

attr(magerman_replace_sgml_characters, "@title") <-
    "Replaces sgml characters"
## --------<<  magerman.replace.sgml.characters:1 ends here



## -------->>  [[file:../harmonizer.src.org::*magerman.replace.proprietary.characters][magerman.replace.proprietary.characters:1]]
##' @eval attr(magerman_replace_proprietary_characters, "@title")
##' @inherit replace_patterns params return
##' @inheritDotParams harmonize_options
##' @return Harmonized table
##' @family magerman
##' @seealso replace_patterns
##'
##' @md
##' @export
magerman_replace_proprietary_characters <- make_alias(replace_patterns
                                                    , patterns = magerman_patterns_proprietary_characters)

attr(magerman_replace_proprietary_characters, "@title") <-
    "Replaces proprietary characters"
## --------<<  magerman.replace.proprietary.characters:1 ends here



## -------->>  [[file:../harmonizer.src.org::*magerman.replace.accented.characters][magerman.replace.accented.characters:1]]
##' @eval attr(magerman_replace_accented_characters, "@title")
##' @inherit replace_patterns params return
##' @inheritDotParams harmonize_options
##' @return Harmonized table
##' @family magerman
##' @seealso replace_patterns
##'
##' @md
##' @export
magerman_replace_accented_characters <- make_alias(replace_patterns
                                                 , patterns = magerman_patterns_accented_characters)

attr(magerman_replace_accented_characters, "@title") <-
    "Replaces accented characters"
## --------<<  magerman.replace.accented.characters:1 ends here



## -------->>  [[file:../harmonizer.src.org::*magerman.remove.special.characters][magerman.remove.special.characters:1]]
##' @eval attr(magerman_remove_special_characters, "@title")
##' @description Removes everything that is not: A-Z; 0-9; “-“; “+”; “’”; “””; “#”;
##' “*”;“@”; “!”; “?”; “/”; “&”; “(“; “)”; “:”; “;”; “,”; “.”; “ “
##' @inherit replace_patterns params return
##' @inheritDotParams harmonize_options
##' @return Harmonized table
##' @family magerman
##' @seealso replace_patterns
##'
##' @md
##' @export
magerman_remove_special_characters <- make_alias(replace_patterns
                                               , patterns = "[^A-Z0-9\\-+'\"#*;@!?/&():;,. ]"
                                               , patterns_type = "regex")

attr(magerman_remove_special_characters, "@title") <-
    "Removes special characters"
## --------<<  magerman.remove.special.characters:1 ends here



## -------->>  [[file:../harmonizer.src.org::*magerman.remove.double.spaces][magerman.remove.double.spaces:1]]
##' @eval attr(magerman_remove_double_spaces, "@title")
##' @inherit replace_patterns params return
##' @inheritDotParams harmonize_options
##' @return Harmonized table
##' @family magerman
##' @seealso replace_patterns
##'
##' @md
##' @export
magerman_remove_double_spaces <- make_alias(replace_patterns
                                          , patterns = "\\s+"
                                          , replacements = " "
                                          , patterns_type = "regex")

attr(magerman_remove_double_spaces, "@title") <-
    "Removes double spaces"
## --------<<  magerman.remove.double.spaces:1 ends here



## -------->>  [[file:../harmonizer.src.org::*magerman.remove.double.quotation.marks.*][magerman.remove.double.quotation.marks.*:1]]
##' @eval attr(magerman_remove_double_quotation_marks_irregularities, "@title")
##' @inherit replace_patterns params return
##' @inheritDotParams harmonize_options
##' @return Harmonized table
##' @family magerman
##' @seealso replace_patterns
##'
##' @md
##' @export
magerman_remove_double_quotation_marks_irregularities <- make_alias(replace_patterns
                                                                  , patterns = c("^\"\"\\s(.*)\"$", "^\"(.*)\\s\"\"$")
                                                                  , replacements = c("\"\"$1\"", "\"$1\"\"")
                                                                  , patterns_type = "regex")

attr(magerman_remove_double_quotation_marks_irregularities, "@title") <-
    "Removes double quotation irregularities"
## --------<<  magerman.remove.double.quotation.marks.*:1 ends here



## -------->>  [[file:../harmonizer.src.org::*magerman.remove.double.quotation.marks.*][magerman.remove.double.quotation.marks.*:3]]
##' @eval attr(magerman_remove_double_quotation_marks_beginning_end, "@title")
##' @inherit replace_patterns params return
##' @inheritDotParams harmonize_options
##' @return Harmonized table
##' @family magerman
##' @seealso replace_patterns
##'
##' @md
##' @export
magerman_remove_double_quotation_marks_beginning_end <- make_alias(replace_patterns
                                                                 , patterns = "^\"\"((?:(?!\"\").)*)\"\"$"
                                                                 , replacements = "$1"
                                                                 , patterns_type = "regex")

attr(magerman_remove_double_quotation_marks_beginning_end, "@title") <-
    "Removes double quotation irregularities"
## --------<<  magerman.remove.double.quotation.marks.*:3 ends here



## -------->>  [[file:../harmonizer.src.org::*magerman.remove.non.alphanumeric.*][magerman.remove.non.alphanumeric.*:1]]
##' @eval attr(magerman_remove_non_alphanumeric_at_the_beginning, "@title")
##' @inherit replace_patterns params return
##' @inheritDotParams harmonize_options
##' @return Harmonized table
##' @family magerman
##' @seealso replace_patterns
##'
##' @md
##' @export
magerman_remove_non_alphanumeric_at_the_beginning <- make_alias(replace_patterns
                                                              , patterns = "^[^A-Z0-9\"@('#!*/]+"
                                                              , patterns_type = "regex")

attr(magerman_remove_non_alphanumeric_at_the_beginning, "@title") <-
    "Removes non alphanumeric characters"


##' @eval attr(magerman_remove_non_alphanumeric_at_the_end, "@title")
##' @inherit replace_patterns params return
##' @inheritDotParams harmonize_options
##' @return Harmonized table
##' @family magerman
##' @seealso replace_patterns
##'
##' @md
##' @export
magerman_remove_non_alphanumeric_at_the_end <- make_alias(replace_patterns
                                                        , patterns = "[^A-Z0-9.'\")]+$"
                                                        , patterns_type = "regex")

attr(magerman_remove_non_alphanumeric_at_the_end, "@title") <-
    "Removes non alphanumeric characters"
## --------<<  magerman.remove.non.alphanumeric.*:1 ends here



## -------->>  [[file:../harmonizer.src.org::*magerman.replace.comma.period.irregularities.*][magerman.replace.comma.period.irregularities.*:1]]
##' @eval attr(magerman_detect_comma_period_irregularities, "@title")
##' @inherit detect_patterns params return
##' @inheritDotParams harmonize_options
##' @return Harmonized table
##' @family magerman
##' @seealso detect_patterns
##'
##' @md
##' @export
magerman_detect_comma_period_irregularities <- make_alias(detect_patterns
                                                        , patterns = c(",([^\\s])", "Patterns with comma not followed by space"
                                                                     , "\\s,", "Patterns with comma preceded by space"
                                                                     , "([^A-Za-z0-9])\\.", "Patterns with period not preceded by a letter or digit") |>
                                                              matrix(byrow = TRUE, ncol = 2) |>
                                                              data.frame()
                                                        , patterns_type = "regex"
                                                        , output_codes_col_name = "comma.period.irregularities.candidates")

attr(magerman_detect_comma_period_irregularities, "@title") <-
    "Detects comma period irregularities"



##' @eval attr(magerman_replace_comma_period_irregularities_all, "@title")
##' @inherit replace_patterns params return
##' @inheritDotParams harmonize_options
##' @return Harmonized table
##' @family magerman
##' @seealso replace_patterns
##'
##' @md
##' @export
magerman_replace_comma_period_irregularities_all <- make_alias(replace_patterns
                                                             , patterns = c(",([^\\s])", ", $1"
                                                                          , "\\s,", ","
                                                                          , "([^A-Za-z0-9])\\.", "$1") |>
                                                                   matrix(byrow = TRUE, ncol = 2) |>
                                                                   data.frame()
                                                             , patterns_type = "regex")

attr(magerman_replace_comma_period_irregularities_all, "@title") <-
    "Replaces comma period irregularities"
## --------<<  magerman.replace.comma.period.irregularities.*:1 ends here



## -------->>  [[file:../harmonizer.src.org::*magerman.replace.comma.period.irregularities.*][magerman.replace.comma.period.irregularities.*:3]]
##' @eval attr(magerman_replace_comma_period_irregularities, "@title")
##' @inherit replace_patterns params return
##' @inheritDotParams harmonize_options
##' @return Harmonized table
##' @family magerman
##' @seealso replace_patterns
##'
##' @md
##' @export
magerman_replace_comma_period_irregularities <- make_alias(replace_patterns
                                                         , patterns = list(magerman_patterns_comma_followed_by_space
                                                                         , magerman_patterns_comma_preceded_by_space
                                                                         , magerman_patterns_periods) |>
                                                               data.table::rbindlist()
                                                         , patterns_type_col = 3)

attr(magerman_replace_comma_period_irregularities, "@title") <-
    "Replaces comma period irregularities"
## --------<<  magerman.replace.comma.period.irregularities.*:3 ends here



## -------->>  [[file:../harmonizer.src.org::*Detect and replace legal forms][Detect and replace legal forms:1]]
##' @eval attr(magerman_detect_legal_form_end, "@title")
##' @inherit detect_patterns params return
##' @inheritDotParams harmonize_options
##' @return Harmonized table
##' @family magerman
##' @seealso detect_patterns
##'
##' @md
##' @export
magerman_detect_legal_form_end <- make_alias(detect_patterns
                                           , patterns = magerman_patterns_legal_form_end
                                           , patterns_type = "ends"
                                           , patterns_codes_col = 3
                                           , return_only_codes = FALSE
                                           , return_only_first_detected_code = TRUE
                                           , patterns_codes = NULL
                                           , no_match_code = NULL
                                           , merge_existing_codes = "replace_empty"
                                           , output_codes_col_name = "{col_name_}legal_form")

attr(magerman_detect_legal_form_end, "@title") <-
    "Detects legal form"






##' @eval attr(magerman_replace_legal_form_end, "@title")
##' @inherit replace_patterns params return
##' @inheritDotParams harmonize_options
##' @return Harmonized table
##' @family magerman
##' @seealso replace_patterns
##'
##' @md
##' @export
magerman_replace_legal_form_end <- make_alias(replace_patterns
                                            , patterns = magerman_patterns_legal_form_end
                                            , patterns_type = "ends")

attr(magerman_replace_legal_form_end, "@title") <-
    "Replaces legal form"






##' @eval attr(magerman_detect_legal_form_beginning, "@title")
##' @inherit detect_patterns params return
##' @inheritDotParams harmonize_options
##' @return Harmonized table
##' @family magerman
##' @seealso detect_patterns
##'
##' @md
##' @export
magerman_detect_legal_form_beginning <- make_alias(detect_patterns
                                                 , patterns = data.table(pattern = "KABUSHIKI KAISHA"
                                                                       , legal.form = "KAISHA")
                                                 , patterns_type = "begins"
                                                 , merge_existing_codes = "replace_empty"
                                                 , return_only_first_detected_code = TRUE
                                                 , output_codes_col_name = "{col_name_}legal_form")

attr(magerman_detect_legal_form_beginning, "@title") <-
    "Detects legal form"







##' @eval attr(magerman_replace_legal_form_beginning, "@title")
##' @inherit replace_patterns params return
##' @inheritDotParams harmonize_options
##' @return Harmonized table
##' @family magerman
##' @seealso replace_patterns
##'
##' @md
##' @export
magerman_replace_legal_form_beginning <- make_alias(replace_patterns
                                                  , patterns = "KABUSHIKI KAISHA"
                                                  , patterns_type = "begins")

attr(magerman_replace_legal_form_beginning, "@title") <-
    "Replaces legal form"







##' @eval attr(magerman_detect_legal_form_middle, "@title")
##' @inherit detect_patterns params return
##' @inheritDotParams harmonize_options
##' @return Harmonized table
##' @family magerman
##' @seealso detect_patterns
##'
##' @md
##' @export
magerman_detect_legal_form_middle <- make_alias(detect_patterns
                                              , patterns = magerman_patterns_legal_form_middle
                                              , patterns_codes_col = 3
                                              , patterns_type = "fixed"
                                              , return_only_first_detected_code = TRUE
                                              , merge_existing_codes = "replace_empty"
                                              , output_codes_col_name = "{col_name_}legal_form")

attr(magerman_detect_legal_form_middle, "@title") <-
    "Detects legal form"




##' @eval attr(magerman_replace_legal_form_middle, "@title")
##' @inherit replace_patterns params return
##' @inheritDotParams harmonize_options
##' @return Harmonized table
##' @family magerman
##' @seealso replace_patterns
##'
##' @md
##' @export
magerman_replace_legal_form_middle <- make_alias(replace_patterns
                                               , patterns = magerman_patterns_legal_form_middle
                                               , patterns_type = "fixed")

attr(magerman_replace_legal_form_middle, "@title") <-
    "Replace legal form"



##' @eval attr(magerman_detect_legal_form, "@title")
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


attr(magerman_detect_legal_form, "@title") <-
    "Detect legal form"

##' @eval attr(magerman_remove_legal_form, "@title")
##' @param x table
##' @inheritDotParams harmonize_options
##' @return Harmonized table
##'
##' @export
magerman_remove_legal_form <- function(x, ...) {
    legal_form_end_rows <-
        magerman_detect_legal_form_end(x
                                     , merge_existing_codes = "replace_all"
                                     , return_only_codes = TRUE
                                     , patterns_codes = TRUE
                                     , no_match_code = FALSE)
    x |>
        magerman_replace_legal_form_beginning(rows = !legal_form_end_rows) |>
        magerman_replace_legal_form_middle(rows = !legal_form_end_rows) |>
        magerman_replace_legal_form_end()        
}

attr(magerman_remove_legal_form, "@title") <- "Removes legal form"



##' @eval attr(magerman_remove_legal_form_and_clean, "@title")
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

attr(magerman_remove_legal_form_and_clean, "@title") <- "Removes legal form"
## --------<<  Detect and replace legal forms:1 ends here



## -------->>  [[file:../harmonizer.src.org::*Common Words][Common Words:1]]
##' @eval attr(magerman_remove_common_words_at_the_end, "@title")
##' @inherit replace_patterns params return
##' @inheritDotParams harmonize_options
##' @return Harmonized table
##' @family magerman
##' @seealso replace_patterns
##'
##' @md
##' @export
magerman_remove_common_words_at_the_end <- make_alias(replace_patterns
                                                    , patterns = magerman_patterns_common_words_at_the_end
                                                    , patterns_type = "ends")

attr(magerman_remove_common_words_at_the_end, "@title") <-
    "Removes common words"



##' @eval attr(magerman_remove_common_words_at_the_beginning, "@title")
##' @inherit replace_patterns params return
##' @inheritDotParams harmonize_options
##' @return Harmonized table
##' @family magerman
##' @seealso replace_patterns
##'
##' @md
##' @export
magerman_remove_common_words_at_the_beginning <- make_alias(replace_patterns
                                                          , patterns = magerman_patterns_common_words_at_the_beginning
                                                          , patterns_type = "begins")

attr(magerman_remove_common_words_at_the_beginning, "@title") <-
    "Removes common words"




##' @eval attr(magerman_remove_common_words_anywhere, "@title")
##' @inherit replace_patterns params return
##' @inheritDotParams harmonize_options
##' @return Harmonized table
##' @family magerman
##' @seealso replace_patterns
##'
##' @md
##' @export
magerman_remove_common_words_anywhere <- make_alias(replace_patterns
                                                  , patterns = magerman_patterns_common_words_anywhere
                                                  , patterns_type = "fixed")

attr(magerman_remove_common_words_anywhere, "@title") <-
    "Removes common words"


##' @eval attr(magerman_remove_common_words, "@title")
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

attr(magerman_remove_common_words, "@title") <- "Removes common words"
## --------<<  Common Words:1 ends here



## -------->>  [[file:../harmonizer.src.org::*Spelling Variation][Spelling Variation:1]]
##' @eval attr(magerman_replace_spelling_variation, "@title")
##' @inherit replace_patterns params return
##' @inheritDotParams harmonize_options
##' @return Harmonized table
##' @family magerman
##' @seealso replace_patterns
##'
##' @md
##' @export
magerman_replace_spelling_variation <- make_alias(replace_patterns
                                                , patterns = magerman_patterns_spelling_variation)

attr(magerman_replace_spelling_variation, "@title") <-
    "Replaces spelling variation"
## --------<<  Spelling Variation:1 ends here



## -------->>  [[file:../harmonizer.src.org::*Condensing][Condensing:1]]
##' @eval attr(magerman_condense, "@title")
##' @inherit replace_patterns params return
##' @inheritDotParams harmonize_options
##' @return Harmonized table
##' @family magerman
##' @seealso replace_patterns
##'
##' @md
##' @export
magerman_condense <- make_alias(replace_patterns
                              , patterns = "[^a-zA-Z0-9]+"
                              , patterns_type = "regex")

attr(magerman_condense, "@title") <-
    "Condenses string"
## --------<<  Condensing:1 ends here



## -------->>  [[file:../harmonizer.src.org::*Umlaut Harmonization][Umlaut Harmonization:1]]
##' @eval attr(magerman_detect_umlaut, "@title")
##' @inherit detect_patterns params return
##' @inheritDotParams harmonize_options
##' @return Harmonized table
##' @family magerman
##' @seealso detect_patterns
##'
##' @md
##' @export
magerman_detect_umlaut <- make_alias(detect_patterns
                                   , patterns = magerman_patterns_umlaut
                                   , patterns_type = "fixed"
                                   , patterns_codes = TRUE
                                   , no_match_code = FALSE
                                   , output_codes_col_name = "{col_name_}has_umlaut"
                                   , return_only_first_detected_code = TRUE
                                   , return_only_codes = FALSE)

attr(magerman_detect_umlaut, "@title") <-
    "Detects umlauts"


##' @eval attr(magerman_replace_umlaut, "@title")
##' @param x Data
##' @param has_umlaut_col Column with logical values indicating weather a corresponding string has an umlaut. Default is NULL so it detects is automatically first
##' @param drop_has_umlaut_col Whether to drop `has_umlaut_col`. Default is FALSE
##' @param replace_accented_characters Whether to replace accented characters first. Default is FALSE
##' @inheritDotParams harmonize_options
##' @return Harmonized table
##'
##' @md
##' @export
magerman_replace_umlaut <- function(x
                                  , has_umlaut_col = NULL
                                  , drop_has_umlaut_col = TRUE
                                  , replace_accented_characters = FALSE
                                  , ...) {
    ## get x vector...
    x_vector <- get_target(x)
    ## identify names with umlauts
    if (!is.null(has_umlaut_col) & !is.atomic(x)) {
        has_umlaut <-
            get_target(x, col = has_umlaut_col, output_placement = "replace_col")
        checkmate::assert_logical(has_umlaut)
        ## drop has_umlaut_col
        if (isTRUE(drop_has_umlaut_col)) {
            x[[has_umlaut_col]] <- NULL
            ## make atomic just if only one col
            if(x_width(x) == 1) x <- x[[1]]
        }
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

attr(magerman_replace_umlaut, "@title") <- "Replaces Umlauts"
## --------<<  Umlaut Harmonization:1 ends here



## -------->>  [[file:../harmonizer.src.org::*Combined Magerman Procedures][Combined Magerman Procedures:1]]
##' Harmonizes strings using exact procedures described in Magerman et al. 2009.
##' @param x table or vector
##' @param detect_legal_form Whether to detect legal forms. Default is FALSE
##' @param append_output_copy_before_common_words_removal Whether to save harmonized column before `common.words.removal` procedure. Default is FALSE
##' @inheritDotParams harmonize
##' @return Harmonized table
##'
##' @references Magerman et al., 2006 - Data Production Methods for Harmonized Patent Statistics: Patentee Name Harmonization
##'
##' @md
##' @export
harmonize_magerman <- function(x
                             , detect_legal_form = FALSE
                             , append_output_copy_before_common_words_removal = FALSE
                             , ...) {
    magerman_procedures <- magerman_procedures_list
    ## do some tweaks on magerman_procedures
    if (!detect_legal_form) {
        is_magerman_detect_legal_form <- 
            sapply(magerman_procedures
                 , \(p) p[[1]] == "magerman_detect_legal_form")
        magerman_procedures <-
            magerman_procedures[!is_magerman_detect_legal_form]
    }
    if (append_output_copy_before_common_words_removal) {
        which_is_magerman_remove_legal_form_and_clean <-
            sapply(magerman_procedures
                 , \(p) p[[1]] == "magerman_remove_legal_form_and_clean") |>
            which()
        magerman_procedures[[which_is_magerman_remove_legal_form_and_clean]] <-
            c(magerman_procedures[[which_is_magerman_remove_legal_form_and_clean]]
            , list(append_output_copy = TRUE
                 , output_copy_col_name = "{col_name_}before_common_words_removal"))
    }
    harmonize(x, magerman_procedures, ...)
}
## --------<<  Combined Magerman Procedures:1 ends here


