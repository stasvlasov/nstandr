## -------->>  [[file:../nstandr.src.org::*Characters][Characters:1]]
##' @eval make_roxy_tags(magerman_detect_characters)
##' @inherit detect_patterns params return
##' @inheritDotParams standardize_options
##' @family magerman
##'
##' @md
##' @export
magerman_detect_characters <- make_alias(detect_patterns
                                       , patterns = magerman_patterns_detect_characters
                                       , patterns_type = "regex"
                                       , output_codes_col_name = "characters_cleaning_candidates"
                                       , .title = "Detect candidates for characters that need to be cleaned"
                                       , .description = "The function below can be used to detect candidates for characters that need to be cleaned based on the patterns (see table in the details)."
                                       , .tables = list(
                                             list("magerman_patterns_detect_characters"
                                                , title = "Patterns used to identify classes of characters to clean."
                                                , pp = "23-25")
                                         )
                                       , .pp = "23-25"
                                       , .ref = "magerman2006")
## --------<<  Characters:1 ends here



## -------->>  [[file:../nstandr.src.org::*magerman.remove.html.codes][magerman.remove.html.codes:1]]
##' @eval make_roxy_tags(magerman_remove_html_codes)
##' @inherit replace_patterns params return
##' @inheritDotParams standardize_options
##' @family magerman
##'
##' @md
##' @export
magerman_remove_html_codes <- make_alias(replace_patterns
                                       , patterns = "<BR>"
                                       , replacements = " "
                                       , .title = "Removes html codes"
                                       , .description = "Original authors did search for all html tags but only found the `<BR>`. So in general all html tags should be considered. This procedure is for reproducibility only."
                                       , .example = "`<BR>` -> ' '"
                                       , .pp = "23"
                                       , .ref = "magerman2006")
## --------<<  magerman.remove.html.codes:1 ends here



## -------->>  [[file:../nstandr.src.org::*magerman.replace.sgml.characters][magerman.replace.sgml.characters:1]]
##' @eval make_roxy_tags(magerman_replace_sgml_characters)
##' @inherit replace_patterns params return
##' @inheritDotParams standardize_options
##' @family magerman
##'
##' @md
##' @export
magerman_replace_sgml_characters <- make_alias(replace_patterns
                                             , patterns = magerman_patterns_sgml_characters
                                             , .title = "Replaces sgml characters"
                                             , .description = "This is original procedure based on patterns identified with original PATSTAT data. I.e. it is intended for reproducibility purposes. Use `magerman_detect_characters` to identify similar patterns in your own data."
                                             , .example = "&AMP;EXCL; -> !"
                                             , .tables = list(
                                                   list("magerman_patterns_sgml_characters"
                                                      , title = "SGML codes and their ASCII/ANSI equivalent"
                                                      , no = "7"
                                                      , pp = "24")
                                               )
                                             , .pp = "24"
                                             , .ref = "magerman2006")
## --------<<  magerman.replace.sgml.characters:1 ends here



## -------->>  [[file:../nstandr.src.org::*magerman.replace.proprietary.characters][magerman.replace.proprietary.characters:1]]
##' @eval make_roxy_tags(magerman_replace_proprietary_characters)
##' @inherit replace_patterns params return
##' @inheritDotParams standardize_options
##' @family magerman
##'
##' @md
##' @export
magerman_replace_proprietary_characters <- make_alias(replace_patterns
                                                    , patterns = magerman_patterns_proprietary_characters
                                                    , .title = "Replaces proprietary characters"
                                                    , .description = "This is original procedure based on patterns identified with original PATSTAT data. I.e. it is intended for reproducibility purposes. Use `magerman_detect_characters` to identify similar patterns in your own data."
                                                    ## , .example = "{UMLAUT OVER (A)} -> Ä"
                                                    , .example = "{UMLAUT OVER (A)} -> \u00C4"
                                                    , .tables = list(
                                                          list("magerman_patterns_proprietary_characters"
                                                             , title = "Proprietary character codes and their ASCII/ANSI equivalent"
                                                             , no = "8"
                                                             , pp = "25")
                                                      )
                                                    , .pp = "25"
                                                    , .ref = "magerman2006")
## --------<<  magerman.replace.proprietary.characters:1 ends here



## -------->>  [[file:../nstandr.src.org::*magerman.replace.accented.characters][magerman.replace.accented.characters:1]]
##' @eval make_roxy_tags(magerman_replace_accented_characters)
##' @inherit replace_patterns params return
##' @inheritDotParams standardize_options
##' @family magerman
##'
##' @md
##' @export
magerman_replace_accented_characters <- make_alias(replace_patterns
                                                 , patterns = magerman_patterns_accented_characters
                                                 , .title = "Replaces accented characters"
                                                 , .description = "This is original procedure based on patterns identified with original PATSTAT data. I.e. it is intended for reproducibility purposes. Use `magerman_detect_characters` to identify similar patterns in your own data."
                                                   ## , .example = "Æ -> AE"
                                                   , .example = "\u00C6 -> AE"
                                                 , .tables = list(
                                                       list("magerman_patterns_accented_characters"
                                                          , title = "Accented characters and their unaccented equivalent"
                                                          , no = "9"
                                                          , pp = "26-27")
                                                   )
                                                 , .pp = "26-27"
                                                 , .ref = "magerman2006")
## --------<<  magerman.replace.accented.characters:1 ends here



## -------->>  [[file:../nstandr.src.org::*magerman.remove.special.characters][magerman.remove.special.characters:1]]
##' @eval make_roxy_tags(magerman_remove_special_characters)
##' @inherit replace_patterns params return
##' @inheritDotParams standardize_options
##' @family magerman
##'
##' @md
##' @export
magerman_remove_special_characters <- make_alias(replace_patterns
                                               , patterns = "[^A-Z0-9\\-+'\"#*;@!?/&():;,. ]"
                                               , patterns_type = "regex"
                                               , .title = "Removes special characters"
                                               , .description = "Removes everything that is not: A-Z; 0-9; \"-\"; \"+\"; \"'\"; \"\"\"; \"#\"; ##' \"*\";\"@\"; \"!\"; \"?\"; \"/\"; \"&\"; \"(\"; \")\"; \":\"; \";\"; \",\"; \".\"; \" \""
                                               ## , .example = "'©%^_' -> ' '"
                                               , .example = "'\u00A9%^_' -> ' '"
                                               , .pp = "27"
                                               , .ref = "magerman2006")
## --------<<  magerman.remove.special.characters:1 ends here



## -------->>  [[file:../nstandr.src.org::*magerman.remove.double.spaces][magerman.remove.double.spaces:1]]
##' @eval make_roxy_tags(magerman_remove_double_spaces)
##' @inherit replace_patterns params return
##' @inheritDotParams standardize_options
##' @family magerman
##'
##' @md
##' @export
magerman_remove_double_spaces <- make_alias(replace_patterns
                                          , patterns = "\\s+"
                                          , replacements = " "
                                          , patterns_type = "regex"
                                          , .title = "Removes double spaces"
                                          , .example = "'  ' -> ' '"
                                          , .pp = "27-28"
                                          , .ref = "magerman2006")
## --------<<  magerman.remove.double.spaces:1 ends here



## -------->>  [[file:../nstandr.src.org::*magerman.remove.double.quotation.marks.*][magerman.remove.double.quotation.marks.*:1]]
##' @eval make_roxy_tags(magerman_remove_double_quotation_marks_irregularities)
##' @inherit replace_patterns params return
##' @inheritDotParams standardize_options
##' @family magerman
##'
##' @md
##' @export
magerman_remove_double_quotation_marks_irregularities <- make_alias(replace_patterns
                                                                  , patterns = c("^\"\"\\s(.*)\"$", "^\"(.*)\\s\"\"$")
                                                                  , replacements = c("\"\"$1\"", "\"$1\"\"")
                                                                  , patterns_type = "regex"
                                                                  , .title = "Removes double quotation irregularities"
                                                                  , .example = "\"\"IBM\" -> \"IBM\""
                                                                  , .pp = "28"
                                                                  , .ref = "magerman2006")
## --------<<  magerman.remove.double.quotation.marks.*:1 ends here



## -------->>  [[file:../nstandr.src.org::*magerman.remove.double.quotation.marks.*][magerman.remove.double.quotation.marks.*:3]]
##' @eval make_roxy_tags(magerman_remove_double_quotation_marks_beginning_end)
##' @inherit replace_patterns params return
##' @inheritDotParams standardize_options
##' @family magerman
##'
##' @md
##' @export
magerman_remove_double_quotation_marks_beginning_end <- make_alias(replace_patterns
                                                                 , patterns = "^\"\"((?:(?!\"\").)*)\"\"$"
                                                                 , replacements = "$1"
                                                                 , patterns_type = "regex"
                                                                 , .title = "Removes double quotation irregularities"
                                                                 , .description = ""
                                                                 , .example = "\"\"IBM Co.\"\" -> IBM Co."
                                                                 , .pp = "28-29"
                                                                 , .ref = "magerman2006")
## --------<<  magerman.remove.double.quotation.marks.*:3 ends here



## -------->>  [[file:../nstandr.src.org::*magerman.remove.non.alphanumeric.*][magerman.remove.non.alphanumeric.*:1]]
##' @eval make_roxy_tags(magerman_remove_non_alphanumeric_at_the_beginning)
##' @inherit replace_patterns params return
##' @inheritDotParams standardize_options
##' @family magerman
##'
##' @md
##' @export
magerman_remove_non_alphanumeric_at_the_beginning <- make_alias(replace_patterns
                                                              , patterns = "^[^A-Z0-9\"@('#!*/]+"
                                                              , patterns_type = "regex"
                                                              , .title = "Removes non alphanumeric characters at the beginning of a name"
                                                              , .description = ""
                                                              , .example = ".IBM Co. -> IBM Co."
                                                              , .pp = "29"
                                                              , .ref = "magerman2006")

##' @eval make_roxy_tags(magerman_remove_non_alphanumeric_at_the_end)
##' @inherit replace_patterns params return
##' @inheritDotParams standardize_options
##' @family magerman
##'
##' @md
##' @export
magerman_remove_non_alphanumeric_at_the_end <- make_alias(replace_patterns
                                                        , patterns = c(" DITE[:,]$"
                                                                      ,"DITE :$"
                                                                      ,"[^A-Z0-9.'\")]+$")
                                                        , patterns_type = "regex"
                                                        , .title = "Removes non alphanumeric characters at the end of a name"
                                                        , .description = ""
                                                        , .example = "IBM Co., DITE : -> IBM Co."
                                                        , .pp = "29-30"
                                                        , .ref = "magerman2006")
## --------<<  magerman.remove.non.alphanumeric.*:1 ends here



## -------->>  [[file:../nstandr.src.org::*magerman.replace.comma.period.irregularities.*][magerman.replace.comma.period.irregularities.*:1]]
##' @eval make_roxy_tags(magerman_detect_comma_period_irregularities)
##' @inherit detect_patterns params return
##' @inheritDotParams standardize_options
##' @family magerman
##'
##' @md
##' @export
magerman_detect_comma_period_irregularities <-
    make_alias(detect_patterns
             , patterns = magerman_patterns_detect_comma_and_period_irregularities
             , patterns_type = "regex"
             , output_codes_col_name = "comma_period_irregularities_candidates"
             , .title = "Detects comma period irregularities"
             , .description = "This procedure applies the original method for detection of comma and period irregularities."
             , .tables = list(
                   list("magerman_patterns_detect_comma_and_period_irregularities"
                      , title = "Patterns used to identify comma and period irregularities."
                      , pp = "30-32")
               )
             , .pp = "30-32"
             , .ref = "magerman2006")





##' @eval make_roxy_tags(magerman_replace_comma_period_irregularities_all)
##' @inherit replace_patterns params return
##' @inheritDotParams standardize_options
##' @family magerman
##'
##' @md
##' @export
magerman_replace_comma_period_irregularities_all <- make_alias(replace_patterns
                                                             , patterns = c(",([^\\s])", ", $1"
                                                                          , "\\s,", ","
                                                                          , "([^A-Za-z0-9])\\.", "$1") |>
                                                                   matrix(byrow = TRUE, ncol = 2) |>
                                                                   data.frame()
                                                             , patterns_type = "regex"

                                                             , .title = "Replaces comma and period irregularities"
                                                             , .example = ",IBM , Co.. -> IBM, Co."
                                                             , .pp = "30-32"
                                                             , .ref = "magerman2006")
## --------<<  magerman.replace.comma.period.irregularities.*:1 ends here



## -------->>  [[file:../nstandr.src.org::*magerman.replace.comma.period.irregularities.*][magerman.replace.comma.period.irregularities.*:3]]
##' @eval make_roxy_tags(magerman_replace_comma_period_irregularities)
##' @inherit replace_patterns params return
##' @inheritDotParams standardize_options
##' @family magerman
##'
##' @md
##' @export
magerman_replace_comma_period_irregularities <-
    make_alias(replace_patterns
             , patterns = list(magerman_patterns_comma_followed_by_space
                             , magerman_patterns_comma_preceded_by_space
                             , magerman_patterns_periods) |>
                   data.table::rbindlist()
             , patterns_type_col = 3
             , .title = "Replaces comma period irregularities"
             , .description = ""
             , .example = "' CO.,INC.' -> ' CO., INC.'"
             , .tables = list(
                   list("magerman_patterns_comma_followed_by_space"
                      , title = "Patterns with comma not followed by space"
                      , no = "10"
                      , pp = "30-31")
                  ,list("magerman_patterns_comma_preceded_by_space"
                      , title = "Patterns with comma preceded by space"
                      , no = "11"
                      , pp = "31")
                  ,list("magerman_patterns_periods"
                      , title = "Patterns with period not preceded by a letter or digit"
                      , no = "12"
                      , pp = "32")
               )
             , .pp = "30-32"
             , .ref = "magerman2006")
## --------<<  magerman.replace.comma.period.irregularities.*:3 ends here



## -------->>  [[file:../nstandr.src.org::*Detect and replace legal forms][Detect and replace legal forms:1]]
##' @eval make_roxy_tags(magerman_detect_legal_form_end)
##' @inherit detect_patterns params return
##' @inheritDotParams standardize_options
##' @family magerman
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
                                           , output_codes_col_name = "{col_name_}legal_form"
                                           , .title = "Detects legal form at the end of a name"
                                           , .tables = list(
                                                 list("magerman_patterns_legal_form_end"
                                                    , title = "All search and replace statements for all legal forms to be removed at the end of a name"
                                                    , no = "Appendix 2"
                                                    , pp = "56-73")
                                             )
                                           , .pp = "33-36"
                                           , .ref = "magerman2006")









##' @eval make_roxy_tags(magerman_replace_legal_form_end)
##' @inherit replace_patterns params return
##' @inheritDotParams standardize_options
##' @family magerman
##'
##' @md
##' @export
magerman_replace_legal_form_end <- make_alias(replace_patterns
                                            , patterns = magerman_patterns_legal_form_end
                                            , patterns_type = "ends"
                                            , .title = "Replaces legal form at the end of a name"
                                            , .example = "IBM GMBH & CO., AG -> IBM & COMPANY"
                                            , .tables = list(
                                                  list("magerman_patterns_legal_form_end"
                                                     , title = "All search and replace statements for all legal forms to be removed at the end of a name"
                                                     , no = "Appendix 2"
                                                     , pp = "56-73")
                                              )
                                            , .pp = "33-36"
                                            , .ref = "magerman2006")



##' @eval make_roxy_tags(magerman_detect_legal_form_beginning)
##' @inherit detect_patterns params return
##' @inheritDotParams standardize_options
##' @family magerman
##'
##' @md
##' @export
magerman_detect_legal_form_beginning <- make_alias(detect_patterns
                                                 , patterns = data.table(pattern = "KABUSHIKI KAISHA"
                                                                       , legal.form = "KAISHA")
                                                 , patterns_type = "begins"
                                                 , merge_existing_codes = "replace_empty"
                                                 , return_only_first_detected_code = TRUE
                                                 , output_codes_col_name = "{col_name_}legal_form"
                                                 , .title = "Detects legal form at the beginning of a name"
                                                 , .pp = "38"
                                                 , .ref = "magerman2006")









##' @eval make_roxy_tags(magerman_replace_legal_form_beginning)
##' @inherit replace_patterns params return
##' @inheritDotParams standardize_options
##' @return standardized names table
##' @family magerman
##' @seealso replace_patterns
##'
##' @md
##' @export
magerman_replace_legal_form_beginning <- make_alias(replace_patterns
                                                  , patterns = "KABUSHIKI KAISHA"
                                                  , patterns_type = "begins"
                                                  , .title = "Replaces legal form at the beginning of a name"
                                                  , .example = "IBM KABUSHIKI KAISHA -> IBM"
                                                  , .pp = "38"
                                                  , .ref = "magerman2006")



##' @eval make_roxy_tags(magerman_detect_legal_form_middle)
##' @inherit detect_patterns params return
##' @inheritDotParams standardize_options
##' @family magerman
##'
##' @md
##' @export
magerman_detect_legal_form_middle <- make_alias(detect_patterns
                                              , patterns = magerman_patterns_legal_form_middle
                                              , patterns_codes_col = 3
                                              , patterns_type = "fixed"
                                              , return_only_first_detected_code = TRUE
                                              , merge_existing_codes = "replace_empty"
                                              , output_codes_col_name = "{col_name_}legal_form"
                                              , .title = "Detects legal form in the middle of a name"
                                              , .tables = list(
                                                    list("magerman_patterns_legal_form_middle"
                                                       , title = "Search and replace statements for legal form GMBH to be removed anywhere in name"
                                                       , no = "18"
                                                       , pp = "38-39")
                                                )
                                              , .pp = "38-39"
                                              , .ref = "magerman2006")







##' @eval make_roxy_tags(magerman_replace_legal_form_middle)
##' @inherit replace_patterns params return
##' @inheritDotParams standardize_options
##' @family magerman
##'
##' @md
##' @export
magerman_replace_legal_form_middle <- make_alias(replace_patterns
                                               , patterns = magerman_patterns_legal_form_middle
                                               , patterns_type = "fixed"
                                               , .title = "Replace legal form in the middle of a name"
                                               , .example = "IBM GMBH & CO.K.G. -> IBM & COMPANY"
                                               , .tables = list(
                                                     list("magerman_patterns_legal_form_middle"
                                                        , title = "Search and replace statements for legal form GMBH to be removed anywhere in name"
                                                        , no = "18"
                                                        , pp = "38-39")
                                                 )
                                               , .pp = "38-39"
                                               , .ref = "magerman2006")



##' @eval make_roxy_tags(magerman_detect_legal_form)
##' @param x table
##' @inheritDotParams standardize_options
##' @return standardized names table
##'
##' @export
magerman_detect_legal_form <- function(x, ...) {
    x |>
        magerman_detect_legal_form_end() |>
        magerman_detect_legal_form_beginning() |>
        magerman_detect_legal_form_middle()
}

add_attr(magerman_detect_legal_form
       , .title = "Detect legal form"
       , .combined = c("magerman_detect_legal_form_end"
                     , "magerman_detect_legal_form_beginning"
                     , "magerman_detect_legal_form_middle")
       , .tables = list(
             list("magerman_patterns_legal_form_middle"
                , title = "Search and replace statements for legal form GMBH to be removed anywhere in name"
                , no = "18"
                , pp = "38-39")
           , list("magerman_patterns_legal_form_end"
                , title = "All search and replace statements for all legal forms to be removed at the end of a name"
                , no = "Appendix 2"
                , pp = "56-73")
         )
       , .pp = "33-40"
       , .ref = "magerman2006")


##' @title Detect legal form
##' @inherit magerman_detect_legal_form
##' @inheritDotParams standardize_options
##' @export
detect_legal_form <- magerman_detect_legal_form




##' @eval make_roxy_tags(magerman_remove_legal_form)
##' @param x table
##' @inheritDotParams standardize_options
##' @return standardized names table
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

add_attr(magerman_remove_legal_form
       , .title = "Removes legal form"
       , .example = "IBM INC. -> IBM"
       , .tables = list(
             list("magerman_patterns_legal_form_middle"
                , title = "Search and replace statements for legal form GMBH to be removed anywhere in name"
                , no = "18"
                , pp = "38-39")
           , list("magerman_patterns_legal_form_end"
                , title = "All search and replace statements for all legal forms to be removed at the end of a name"
                , no = "Appendix 2"
                , pp = "56-73")
         )
       , .pp = "33-40"
       , .ref = "magerman2006")

##' @eval make_roxy_tags(magerman_remove_legal_form_and_clean)
##' @param x table
##' @inheritDotParams standardize_options
##' @return standardized names table
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

add_attr(magerman_remove_legal_form_and_clean
       , .title = "Removes legal form"
       , .example = "IBM INC. -> IBM"
       , .combined = c("magerman_remove_legal_form")
       , .tables = list(
             list("magerman_patterns_legal_form_middle"
                , title = "Search and replace statements for legal form GMBH to be removed anywhere in name"
                , no = "18"
                , pp = "38-39")
           , list("magerman_patterns_legal_form_end"
                , title = "All search and replace statements for all legal forms to be removed at the end of a name"
                , no = "Appendix 2"
                , pp = "56-73")
         )
       , .pp = "33-40"
       , .ref = "magerman2006")
## --------<<  Detect and replace legal forms:1 ends here



## -------->>  [[file:../nstandr.src.org::*Common Words][Common Words:1]]
##' @eval make_roxy_tags(magerman_remove_common_words_at_the_end)
##' @inherit replace_patterns params return
##' @inheritDotParams standardize_options
##' @family magerman
##'
##' @md
##' @export
magerman_remove_common_words_at_the_end <-
    make_alias(replace_patterns
             , patterns = magerman_patterns_common_words_at_the_end
             , patterns_type = "ends"
             , .title = "Removes common words at the end of a name"
             , .example = "3COM CORPORATION -> 3COM"
             , .tables = list(
                   list("magerman_patterns_common_words_at_the_end"
                      , title = "Common company words to be removed at the end of a name"
                      , no = "19"
                      , pp = "40-41")
               )
             , .pp = "40"
             , .ref = "magerman2006")





##' @eval make_roxy_tags(magerman_remove_common_words_at_the_beginning)
##' @inherit replace_patterns params return
##' @inheritDotParams standardize_options
##' @family magerman
##'
##' @md
##' @export
magerman_remove_common_words_at_the_beginning <-
    make_alias(replace_patterns
             , patterns = magerman_patterns_common_words_at_the_beginning
             , patterns_type = "begins"
             , .title = "Removes common words at the beginning of a name"
             , .example = "SOCIETE NOVATEC -> NOVATEC"
             , .tables = list(
                   list("magerman_patterns_common_words_at_the_beginning"
                      , title = "Common company words to be removed at the beginning of a name"
                      , no = "20"
                      , pp = "41")
               )
             , .pp = "41"
             , .ref = "magerman2006")


##' @eval make_roxy_tags(magerman_remove_common_words_anywhere)
##' @inherit replace_patterns params return
##' @inheritDotParams standardize_options
##' @family magerman
##'
##' @md
##' @export
magerman_remove_common_words_anywhere <-
    make_alias(replace_patterns
             , patterns = magerman_patterns_common_words_anywhere
             , patterns_type = "fixed"
             , .title = "Removes common words anywhere in a name"
             , .example = "AMIC COMPANY -> AMIC"
             , .tables = list(
                   list("magerman_patterns_common_words_anywhere"
                      , title = "Common company words to be removed anywhere in a name"
                      , no = "21"
                      , pp = "41")
               )
             , .pp = "41-42"
             , .ref = "magerman2006")





##' @eval make_roxy_tags(magerman_remove_common_words)
##' @param x table
##' @inheritDotParams standardize_options
##' @return standardized names table
##'
##' @md
##' @export
magerman_remove_common_words <- function(x, ...) {
    x |>
        magerman_remove_common_words_at_the_end() |>
        magerman_remove_common_words_at_the_beginning() |>
        magerman_remove_common_words_anywhere()
}

add_attr(magerman_remove_common_words
       , .title = "Remove common words"
       , .combined = c("magerman_remove_common_words_at_the_end"
                      ,"magerman_remove_common_words_at_the_beginning"
                      ,"magerman_remove_common_words_anywhere")
       , .example = "3COM CORPORATION -> 3COM"
       , .tables = list(
             list("magerman_patterns_common_words_at_the_end"
                , title = "Common company words to be removed at the end of a name"
                , no = "19"
                , pp = "40-41")
           , list("magerman_patterns_common_words_at_the_beginning"
                , title = "Common company words to be removed at the beginning of a name"
                , no = "20"
                , pp = "41")
           , list("magerman_patterns_common_words_anywhere"
                , title = "Common company words to be removed anywhere in a name"
                , no = "21"
                , pp = "41")
         )
       , .pp = "40-42"
       , .ref = "magerman2006")
## --------<<  Common Words:1 ends here



## -------->>  [[file:../nstandr.src.org::*Spelling Variation][Spelling Variation:1]]
##' @eval make_roxy_tags(magerman_replace_spelling_variation)
##' @inherit replace_patterns params return
##' @inheritDotParams standardize_options
##' @family magerman
##'
##' @md
##' @export
magerman_replace_spelling_variation <- make_alias(replace_patterns
                                                , patterns = magerman_patterns_spelling_variation
                                                , .title = "Replaces spelling variation"
                                                , .example = "INTERNATIONALE -> INTERNATIONAL"
                                                , .tables = list(
                                                      list("magerman_patterns_spelling_variation"
                                                         , title = "Spelling variations and their standardized equivalent"
                                                         , no = "22"
                                                         , pp = "42-43")
                                                  )
                                                , .pp = "42-43"
                                                , .ref = "magerman2006")
## --------<<  Spelling Variation:1 ends here



## -------->>  [[file:../nstandr.src.org::*Condensing][Condensing:1]]
##' @eval make_roxy_tags(magerman_condense)
##' @inherit replace_patterns params return
##' @inheritDotParams standardize_options
##' @family magerman
##'
##' @md
##' @export
magerman_condense <- make_alias(replace_patterns
                              , patterns = "[^a-zA-Z0-9]+"
                              , patterns_type = "regex"
                              , .title = "Condensing names"
                              , .example = "3 COM -> 3COM"
                              , .pp = "43-44"
                              , .ref = "magerman2006")
## --------<<  Condensing:1 ends here



## -------->>  [[file:../nstandr.src.org::*Umlaut Standardization][Umlaut Standardization:1]]
##' @eval make_roxy_tags(magerman_detect_umlaut)
##' @inherit detect_patterns params return
##' @inheritDotParams standardize_options
##' @family magerman
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
                                   , return_only_codes = FALSE
                                   , .title = "Detect umlauts"
                                     ## , .example = "GLÜHLAMPEN -> GLUEHLAMPEN"
                                   , .example = "GL\u00DCHLAMPEN -> GLUEHLAMPEN"
                                   , .tables = list(
                                         list("magerman_patterns_umlaut"
                                            , title = "Umlaut harmonization replacements"
                                            , pp = "44-45")
                                     )
                                   , .pp = "44-45"
                                   , .ref = "magerman2006")




##' @eval make_roxy_tags(magerman_replace_umlaut)
##' @param x Data
##' @param has_umlaut_col Column with logical values indicating weather a corresponding string has an umlaut. Default is NULL so it detects is automatically first
##' @param drop_has_umlaut_col Whether to drop `has_umlaut_col`. Default is FALSE
##' @param replace_accented_characters Whether to replace accented characters first. Default is FALSE
##' @inheritDotParams standardize_options
##' @return standardized names table
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
                    , formals("standardize_options"))) |>
            as.logical()
    }
    ## replace accented characters
    if (replace_accented_characters) {
        ## do magerman_replace_accented_characters with defauls
        x_vector <-
            do.call(magerman_replace_accented_characters
                  , c(list(x_vector), formals("standardize_options")))
    }
    ## check if there are at least some umlauts
    if (any(has_umlaut)) {
        ## transform umlaut
        ## first "AE", "OE", "UE" -> "A", "O", "U"
        x_standardized <- 
            do.call(replace_patterns
                  , c(list(x_vector)
                    , formals("standardize_options")
                    , list(patterns = magerman_patterns_umlaut
                         , patterns_col = 3
                         , patterns_replacements_col = 2)))
        ## then "A", "O", "U" -> "AE", "OE", "UE"
        x_standardized <- 
            do.call(replace_patterns
                  , c(list(x_standardized)
                    , formals("standardize_options")
                    , list(patterns = magerman_patterns_umlaut
                         , patterns_col = 2
                         , patterns_replacements_col = 3)))
        ## check which one match original umlaut
        x_standardized_keep <-
            x_standardized %in%
            x_standardized[sapply(has_umlaut, isTRUE)]
        ## if does not match umlaut replace with original
        x_standardized[!x_standardized_keep] <-
            x_vector[!x_standardized_keep]
        ## return table
        return(inset_target(x_standardized, x))
    } else {
        return(inset_target(x_vector, x))
    }
}

add_attr(magerman_replace_umlaut
       , .title = "Replaces Umlauts"
       ## , .example = "GLÜHLAMPEN -> GLUEHLAMPEN"
       , .example = "GL\u00DCHLAMPEN -> GLUEHLAMPEN"
       , .tables = list(
             list("magerman_patterns_umlaut"
                , title = "Umlaut harmonization replacements"
                , pp = "44-45")
         )
       , .pp = "44-45"
       , .ref = "magerman2006")
## --------<<  Umlaut Standardization:1 ends here



## -------->>  [[file:../nstandr.src.org::*Combined Magerman Procedures][Combined Magerman Procedures:3]]
##' Standardizes strings using exact procedures described in Magerman et al. 2009.
##' @param x table or vector
##' @param detect_legal_form Whether to detect legal forms. Default is FALSE
##' @param append_output_copy_before_common_words_removal Whether to save standardized column before `common.words.removal` procedure. Default is FALSE
##' @param condense_words Whether to remove all spaces in standard names
##' @inheritDotParams standardize
##' @return standardized names table
##'
##' @references Magerman et al., 2006 - Data Production Methods for Harmonized Patent Statistics: Patentee Name Standardization
##'
##' @aliases standardize_eee_ppat
##'
##' @md
##' @export
standardize_magerman <- function(x
                             , detect_legal_form = FALSE
                             , append_output_copy_before_common_words_removal = FALSE
                             , condense_words = FALSE
                             , ...) {
    magerman_procedures <- nstandr_magerman_procedures_list
    ## do some tweaks on magerman_procedures
    if (!detect_legal_form) {
        ## "magerman_detect_legal_form" 
        magerman_procedures$`Legal forms`$`Detecting legal form` <- NULL
    }
    if (!condense_words) {
        ## "magerman_condense"
        magerman_procedures$`Common words`$Condensing <- NULL
    }
    if (append_output_copy_before_common_words_removal) {
        ## "magerman_remove_legal_form_and_clean"
        magerman_procedures$`Legal forms`$`Removing legal form` <-
            call("magerman_remove_legal_form_and_clean"
               , append_output_copy = TRUE
               , output_copy_col_name = "{col_name_}before_common_words_removal")
    }
    standardize(x, magerman_procedures, ...)
}


##' @inherit standardize_magerman params return
##' @inheritDotParams standardize_options
##' @family magerman
##'
##' @md
##' @export
standardize_eee_ppat <- standardize_magerman

## make condensing words default
formals(standardize_eee_ppat)["condense_words"] <- TRUE
## --------<<  Combined Magerman Procedures:3 ends here


