## -------->>  [[file:../harmonizer.src.org::*Characters][Characters:1]]
##' Detects candidates for characters that need to be cleaned
##' @param x table
##' @inheritDotParams harmonize.detect
##' @return coded table
##'
##' @md
##' @import magrittr
##' @export
magerman_detect_characters <- function(x,
                                       codes.name = "characters.cleaning.candidates",
                                       ...) {
    harmonize.detect(x,
        patterns = c(
            "\\{.+\\}", "propriety coded characters {xxx}",
            "\\[0.+\\]", "propriety coded characters [0xxx]",
            "\\(.+\\)", "propriety coded characters (xxx)",
            "&.+;", "sgml coded characters",
            "<.+>", "html coded characters"
        ) %>%
            matrix(byrow = TRUE, ncol = 2) %>%
            data.frame(),
        patterns.type = "regex",
        codes.name = codes.name,
        ...
    )
}
## --------<<  Characters:1 ends here



## -------->>  [[file:../harmonizer.src.org::*magerman.remove.html.codes][magerman.remove.html.codes:1]]
##' Removes html codes
##' @param x table
##' @inheritDotParams harmonize_replace
##' @return harmonized table
##'
##' @md
##' @export
magerman_remove_html_codes <- function(x, ...) {
    harmonize_replace(x,
        patterns = "<BR>",
        replacements = " ",
        ...
    )
}

## ## Tests
## "bla bla <BR>" %>% magerman.remove.html.codes
## --------<<  magerman.remove.html.codes:1 ends here



## -------->>  [[file:../harmonizer.src.org::*magerman.replace.sgml.characters][magerman.replace.sgml.characters:1]]
##' Replaces sgml characters. Accept both vector and table and return either vector or a table
##' @param x table
##' @inheritDotParams harmonize_replace
##' @return Harmonized table
##'
##' @md
##' @export
magerman_replace_sgml_characters <- function(x, ...) {
    harmonize_replace(x, magerman.patterns.sgml.characters, ...)
}


## test
## test.df <-
## data.frame(
##     c("&AMP;&OACUTE;&SECT; 02937lkjfas;ldjf  &UACUTE;&#8902;&BULL; sdlfkjhhhh ;laskdjf&EXCL;"
##   ,   "&AMP;&OACUTE;&SECT; 02937lkjfas;ldjf  &UACUTE;&#8902;&BULL; sdlfkjhhhh ;laskdjf&EXCL;")
##   , c("swe"
##     , "w3r"))

## test.df %>%
##     harmonize.replace(magerman.patterns.sgml.characters)

## test.df %>% magerman.replace.sgml.characters
## --------<<  magerman.replace.sgml.characters:1 ends here



## -------->>  [[file:../harmonizer.src.org::*magerman.replace.proprietary.characters][magerman.replace.proprietary.characters:1]]
##' Replaces proprietary characters
##' @param x table
##' @inheritDotParams harmonize_replace
##' @return Harmonized table
##'
##' @md
##' @export
magerman_replace_proprietary_characters <- function(x, ...) {
    harmonize_replace(x, magerman.patterns.proprietary.characters, ...)
}

## test
## "&AMP;&OACUTE;&SECT; {UMLAUT OVER (E)} sdlfkjhhhh ;laskdjf&EXCL;" %>%
##   magerman.replace.proprietary.characters
## --------<<  magerman.replace.proprietary.characters:1 ends here



## -------->>  [[file:../harmonizer.src.org::*magerman.replace.accented.characters][magerman.replace.accented.characters:1]]
##' Replaces accented characters
##'
##' Assumes that all characters are in caps.
##' @param x table
##' @inheritDotParams harmonize_replace
##' @return Harmonized table
##'
##' @md
##' @export
magerman_replace_accented_characters <- function(x, ...) {
    harmonize_replace(x, magerman.patterns.accented.characters, ...)
}

## Test
## "ŠŒŽšœžŸ¥µÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝßàáâãäåæçèéêëìíîïðñòóôõöøùúûüýÿ" %>%
##   magerman.replace.accented.characters

## somewhat works:
## [1] "ŠŒŽšœžY¥µAAAAAAAECEEEEIIIIÐNOOOOOØUUUUYßàáâãäåæçèéêëìíîïðñòóôõöøùúûüýÿ"
## --------<<  magerman.replace.accented.characters:1 ends here



## -------->>  [[file:../harmonizer.src.org::*magerman.remove.special.characters][magerman.remove.special.characters:1]]
##' Removes special characters. I.e., everything that is not:
## A-Z; 0-9; “-“; “+”; “’”; “””; “#”; “*”;“@”; “!”; “?”; “/”; “&”; “(“; “)”; “:”; “;”; “,”; “.”; “ “
##' @param x table
##' @inheritDotParams harmonize_replace
##' @return Harmonized table
##'
##' @md
##' @export
magerman_remove_special_characters <- function(x, ...) {
    harmonize_replace(x,
        "[^A-Z0-9\\-+'\"#*;@!?/&():;,. ]",
        patterns.type = "regex",
        ...
    )
}

## test
## "LK \tD©𝍎 ၍\tF:'\";092834!@#$%^&*()_+-\n\t" %>%
##   magerman.remove.special.characters %>%
##   message
## --------<<  magerman.remove.special.characters:1 ends here



## -------->>  [[file:../harmonizer.src.org::*magerman.remove.double.spaces][magerman.remove.double.spaces:1]]
##' Removes punctuation and standardise some symbols. 
##'
##' @param x object
##' @inheritDotParams harmonize_replace
##' @return Harmonized table
##' 
##' @md
##' @import magrittr
##' @export 
cockburn.replace.punctuation <- function(x
                                         , ...) {
  x %>%
    harmonize_replace(patterns = cockburn.patterns.punctuation.and, ...) %>%
    harmonize_replace(patterns = cockburn.patterns.punctuation.the
                    , patterns.type.col = 3, ...) %>%
    ## I swapted patstat with amadeus otherwise Ã²Ã¢ÃªÃ®Ã© will not become oaeie
    harmonize_replace(patterns = cockburn.patterns.punctuation.patstat, ...) %>% 
    harmonize_replace(patterns = cockburn.patterns.punctuation.amadeus, ...) %>%
    harmonize_replace(patterns = cockburn.patterns.punctuation.char, ...)
}

## Test
## c("WESTINGHOUSE, |.?^&*@ ELEC  "
## , "GRACE (W EN R) & CO - Ã²Ã¢ÃªÃ®Ã©"
## , "GRACE (W/R) & CO Ltd.") %>% 
##   cockburn.replace.punctuation
## --------<<  magerman.remove.double.spaces:1 ends here



## -------->>  [[file:../harmonizer.src.org::*magerman.remove.double.quotation.marks.*][magerman.remove.double.quotation.marks.*:1]]
##' Removes double quotation irregularities
##' @param x table
##' @inheritDotParams harmonize_replace
##' @return Harmonized table
##'
##' @md
##' @export
magerman_remove_double_quotation_marks_irregularities <- function(x, ...) {
    harmonize_replace(x,
        patterns = c("^\"\"\\s(.*)\"$", "^\"(.*)\\s\"\"$"),
        replacements = c("\"\"$1\"", "\"$1\"\""),
        patterns.type = "regex",
        ...
    )
}

## Test   magerman.remove.double.quotation.marks.irregularities
## c("\"\" Merry  \"Cristmas\" Love\"\""
## , "\"\"Merry \"\"Cristmas\"\" Love \"\"") %>%
##   magerman.remove.double.quotation.marks.irregularities(bind.x.cols = "all")
## --------<<  magerman.remove.double.quotation.marks.*:1 ends here



## -------->>  [[file:../harmonizer.src.org::*magerman.remove.double.quotation.marks.*][magerman.remove.double.quotation.marks.*:2]]
##' Removes double quotation irregularities
##' @param x table
##' @inheritDotParams harmonize_replace
##' @return Harmonized table
##'
##' @md
##' @export
magerman_remove_double_quotation_marks_beginning_end <- function(x, ...) {
    harmonize_replace(x,
        patterns = "^\"\"((?:(?!\"\").)*)\"\"$",
        replacements = "$1",
        patterns.type = "regex",
        ...
    )
}

## Test magerman.remove.double.quotation.marks.beginning.end
## c("\"\"Merry  \"Cristmas\" Love\"\"" # delete quotes here
## , "\"\"Merry \"\"Cristmas\"\" Love\"\""  # do not delete here
##   ) %>%
##   magerman.remove.double.quotation.marks.beginning.end(bind.x.cols = "all")
## --------<<  magerman.remove.double.quotation.marks.*:2 ends here



## -------->>  [[file:../harmonizer.src.org::*magerman.remove.non.alphanumeric.*][magerman.remove.non.alphanumeric.*:1]]
##' Removes non alphanumeric characters
##' @param x table
##' @inheritDotParams harmonize_replace
##' @return Harmonized table
##'
##' @md
##' @export
magerman_remove_non_alphanumeric_at_the_beginning <- function(x, ...) {
    harmonize_replace(x,
        patterns = "^[^A-Z0-9\"@('#!*/]+",
        patterns.type = "regex",
        ...
    )
}

## Test:
## c("_MSLab Co."
## , "?MSLab Co."
## , ".-:MSLab Co.") %>% magerman.remove.non.alphanumeric.at.the.beginning


##' Removes non alphanumeric characters
##' @param x table
##' @inheritDotParams harmonize_replace
##' @return Harmonized table
##'
##' @md
##' @export
magerman_remove_non_alphanumeric_at_the_end <- function(x, ...) {
    harmonize_replace(x,
        patterns = "[^A-Z0-9.'\")]+$",
        patterns.type = "regex",
        ...
    )
}

## Test:
## c("MSLab Co. :"
## , "MSLab Co.++"
## , "MSLab Co.*&^") %>% magerman.remove.non.alphanumeric.at.the.end
## --------<<  magerman.remove.non.alphanumeric.*:1 ends here



## -------->>  [[file:../harmonizer.src.org::*magerman.replace.comma.period.irregularities.*][magerman.replace.comma.period.irregularities.*:1]]
##' Detects comma period irregularities
##' @param x table
##' @inheritDotParams harmonize.detect
##' @return Harmonized table
##'
##' @md
##' @import magrittr
##' @export
magerman_detect_comma_period_irregularities <- function(x,
                                                        codes.name = "comma.period.irregularities.candidates",
                                                        ...) {
    c(
        ",([^\\s])", "Patterns with comma not followed by space",
        "\\s,", "Patterns with comma preceded by space",
        "([^A-Za-z0-9])\\.", "Patterns with period not preceded by a letter or digit"
    ) %>%
        matrix(byrow = TRUE, ncol = 2) %>%
        data.frame() %>%
        harmonize.detect(x,
            patterns = .,
            patterns.type = "regex",
            codes.name = codes.name,
            ...
        )
}

##' Replaces comma period irregularities
##' @param x table
##' @inheritDotParams harmonize_replace
##' @return Harmonized table
##'
##' @md
##' @import magrittr
##' @export
magerman_replace_comma_period_irregularities_all <- function(x, ...) {
    c(
        ",([^\\s])", ", $1",
        "\\s,", ",",
        "([^A-Za-z0-9])\\.", "$1"
    ) %>%
        matrix(byrow = TRUE, ncol = 2) %>%
        data.frame() %>%
        harmonize_replace(x,
            patterns =  .,
            patterns.type = "regex",
            ...
        )
}

## Test magerman.replace.comma.period.irregularities.all
## "A sentence with .irregular punctuation ,like commas , and periods ." %>%
##  magerman.replace.comma.period.irregularities.all
## --------<<  magerman.replace.comma.period.irregularities.*:1 ends here



## -------->>  [[file:../harmonizer.src.org::*magerman.replace.comma.period.irregularities.*][magerman.replace.comma.period.irregularities.*:2]]
##' Replaces comma period irregularities
##' @param x object
##' @inheritDotParams harmonize_replace
##' @return Harmonized table
##'
##' @md
##' @import magrittr
##' @export
magerman_replace_comma_period_irregularities <- function(x, ...) {
    list(
        magerman.patterns.comma.followed.by.space,
        magerman.patterns.comma.preceded.by.space,
        magerman.patterns.periods
    ) %>%
        rbindlist() %>%
        harmonize_replace(x, ., patterns.type.col = 3, ...)
}

## Test magerman.replace.comma.period.irregularities
## c("MSlab ,INC. ,LTD"
## , "MSlab ,LTD Universe") %>%
##   magerman.replace.comma.period.irregularities(bind.x.cols = "all")
## --------<<  magerman.replace.comma.period.irregularities.*:2 ends here



## -------->>  [[file:../harmonizer.src.org::*Detect and replace legal forms][Detect and replace legal forms:1]]
##' Detects legal form
##' @param x table
##' @inheritDotParams harmonize.detect
##' @return Harmonized table
##'
##' @md
##' @export
magerman_detect_legal_form_end <- function(x, ...) {
    harmonize.detect(x,
        magerman.patterns.legal.form.end,
        patterns.codes.col = 3,
        patterns.type = "ends",
        return.only.first.detected.code = TRUE,
        ...
    )
}

##' Replaces legal form
##' @param x table
##' @inheritDotParams harmonize_replace
##' @return Harmonized table
##'
##' @md
##' @export
magerman_replace_legal_form_end <- function(x, ...) {
    harmonize_replace(x,
        patterns = magerman.patterns.legal.form.end,
        patterns.type = "ends",
        ...
    )
}

##' Detects legal form
##' @param x table
##' @inheritDotParams harmonize.detect
##' @return Harmonized table
##'
##' @md
##' @export
magerman_detect_legal_form_beginning <- function(x, ...) {
    harmonize.detect(x,
        patterns = data.table(
            pattern = "KABUSHIKI KAISHA",
            legal.form = "KAISHA"
        ),
        patterns.type = "begins",
        return.only.first.detected.code = TRUE,
        ...
    )
}

##' Replaces legal form
##' @param x table
##' @inheritDotParams harmonize_replace
##' @return Harmonized table
##'
##' @md
##' @export
magerman_replace_legal_form_beginning <- function(x, ...) {
    harmonize_replace(x,
        patterns = "KABUSHIKI KAISHA",
        patterns.type = "begins",
        ...
    )
}



##' Detects legal form
##' @param x table
##' @inheritDotParams harmonize.detect
##' @return Harmonized table
##'
##' @md
##' @export
magerman_detect_legal_form_middle <- function(x, ...) {
    harmonize.detect(x,
        patterns = magerman.patterns.legal.form.middle,
        patterns.codes.col = 3,
        patterns.type = "fixed",
        return.only.first.detected.code = TRUE,
        ...
    )
}

##' Replaces legal form
##' @param x table
##' @inheritDotParams harmonize_replace
##' @return Harmonized table
##'
##' @md
##' @export
magerman_replace_legal_form_middle <- function(x, ...) {
    harmonize_replace(x,
        patterns = magerman.patterns.legal.form.middle,
        patterns.type = "fixed",
        ...
    )
}


## Test
## c("lksdjf MFG. CO, INC"
## , "MSlab Co."
## , "IBM Corp."
## , "MSlab Co. GMBH & CO.KG lalal"
## , "KABUSHIKI KAISHA MSlab Co. ") %>%
##   toupper %>%
##   magerman_detect_legal_form_end
## --------<<  Detect and replace legal forms:1 ends here



## -------->>  [[file:../harmonizer.src.org::*Detect and replace legal forms][Detect and replace legal forms:2]]
##' Detects legal form
##' @param x table
##' @inheritDotParams magerman_detect_legal_form_end
##' @return Harmonized table
##'
##' @md
##' @import magrittr
##' @export
magerman_detect_legal_form <- function(x, ...) {
    x %>%
        magerman_detect_legal_form_end(...) %>%
        magerman_detect_legal_form_beginning(
            x.codes.col = ncol(.),
            x.codes.update.empty = TRUE
        ) %>%
        magerman_detect_legal_form_middle(
            x.codes.col = ncol(.),
            x.codes.update.empty = TRUE
        )
}

## Test
## c("lksdjf MFG. GMBH CO, INC"
##  , "MSlab Co."
##  , "IBM Corp."
##  , "MSlab Co. GMBH & CO.KG lalal"
##  , "KABUSHIKI KAISHA MSlab Co. ") %>%
##    magerman.detect.legal.form


##' Removes legal form
##' @param x table
##' @return Harmonized table
##'
##' @md
##' @import magrittr
##' @export
magerman_remove_legal_form <- function(x) {
    x %>%
        magerman_detect_legal_form_end(codes.name = "not.to.replace") %>%
        magerman_replace_legal_form_end() %>%
        magerman_replace_legal_form_beginning(
            x.rows = harmonize_is_data_empty(.[[ncol(.)]]),
            x.col.update = TRUE
        ) %>%
        magerman_replace_legal_form_middle(
            x.rows = harmonize_is_data_empty(.[[ncol(.)]]),
            x.col.update = TRUE
            ## drop last col "not.to.replace"
            , return.x.cols = -ncol(.)
        )
}




##' Removes legal form
##' @param x table
##' @return Harmonized table
##'
##' @md
##' @import magrittr
##' @export
magerman_remove_legal_form_and_clean <- function(x) {
    x %>%
        magerman_remove_legal_form() %>%
        harmonize_replace(
            patterns = c("[-;:,&]*\\s*$", "^\\s*"),
            patterns.type = "regex"
        )
}
## --------<<  Detect and replace legal forms:2 ends here



## -------->>  [[file:../harmonizer.src.org::*Common Words][Common Words:1]]
##' Removes common words
##' @param x table
##' @inheritDotParams harmonize_replace
##' @return Harmonized table
##'
##' @md
##' @export
magerman_remove_common_words_at_the_end <- function(x, ...) {
    harmonize_replace(x,
        patterns = magerman.patterns.common.words.at.the.end,
        patterns.type = "ends",
        ...
    )
}

##' Removes common words
##' @param x table
##' @inheritDotParams harmonize_replace
##' @return Harmonized table
##'
##' @md
##' @export
magerman_remove_common_words_at_the_beginning <- function(x, ...) {
    harmonize_replace(x,
        patterns = magerman.patterns.common.words.at.the.beginning,
        patterns.type = "begins",
        ...
    )
}



##' Removes common words
##' @param x table
##' @inheritDotParams harmonize_replace
##' @return Harmonized table
##'
##' @md
##' @export
magerman_remove_common_words_anywhere <- function(x, ...) {
    harmonize_replace(x,
        patterns = magerman.patterns.common.words.anywhere,
        patterns.type = "fixed",
        ...
    )
}


##' Removes common words
##' @param x table
##' @inheritDotParams harmonize_replace
##' @return Harmonized table
##'
##' @md
##' @import magrittr
##' @export
magerman_remove_common_words <- function(x, ...) {
    x %>%
        magerman_remove_common_words_at_the_end(...) %>%
        magerman_remove_common_words_at_the_beginning(...) %>%
        magerman_remove_common_words_anywhere(...)
}


## Test
## c("lksdjf MFG. GMBH CO,; INC"
## , "MSlab Co."
## , "IBM Corp."
## , " MSlab Co. GMBH & CO.KG lalal  "
## , "KABUSHIKI KAISHA MSlab Co.") %>%
##   toupper %>%
##   magerman.remove.legal.form.clean %>%
##   magerman.remove.common.words(return.x.cols.all = TRUE)
## --------<<  Common Words:1 ends here



## -------->>  [[file:../harmonizer.src.org::*Spelling Variation][Spelling Variation:1]]
##' Replaces spelling variation
##' @param x table
##' @inheritDotParams harmonize_replace
##' @return Harmonized table
##'
##' @md
##' @export
magerman_replace_spelling_variation <- function(x, ...) {
    harmonize_replace(x,
        patterns = magerman.patterns.spelling.variation,
        ...
    )
}

## Test
## c("CHEMICALS SYSTEMEN MSlab Ltd."
## , "ELECTRONICS SYSTEMES MSlab Co.") %>%
##   magerman.replace.spelling.variation(return.x.cols.all = TRUE)
## --------<<  Spelling Variation:1 ends here



## -------->>  [[file:../harmonizer.src.org::*Condensing][Condensing:1]]
##' Condenses string
##' @param x table
##' @inheritDotParams harmonize_replace
##' @return Harmonized table
##'
##' @md
##' @export
magerman_condense <- function(x, ...) {
    harmonize_replace(x,
        patterns = "[^a-zA-Z0-9]+",
        patterns.type = "regex",
        ...
    )
}

## Test
## c("lksdjf MFG. GMBH CO,; INC"
## , "MSlab Co."
## , "IBM Corp."
## , " MSlab Co. GMBH & CO.KG lalal  "
## , "KABUSHIKI KAISHA MSlab Co.") %>%
##   magerman.condence(return.x.cols.all = TRUE)
## --------<<  Condensing:1 ends here



## -------->>  [[file:../harmonizer.src.org::*Umlaut Harmonization][Umlaut Harmonization:1]]
##' Detects umlauts
##' @param x table
##' @inheritDotParams harmonize.detect
##' @return Coded table
##'
##' @md
##' @export
magerman_detect_umlaut <- function(x, ...) {
    harmonize.detect(x, ,
        patterns = magerman.patterns.umlaut,
        patterns.codes.col = 4,
        patterns.type = "fixed",
        return.only.first.detected.code = TRUE,
        ...
    )
}



## Test
## c("MÄKARÖNI ETÖ FKÜSNÖ Ltd"
## , "MSLab Co."
## , "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝŸ") %>%
##   magerman.detect.umlaut(return.just.codes = FALSE)


##' Replaces Umlauts
##' @param x Data
##' @param x.umlaut.col Column with logical values indicating weather a corresponding string has an umlaut. Default is NULL so it detects is automatically first
##' @param drop.umlaut.col Whether to drop `umlaut.col`. Default is FALSE
##' @param replace.accented.characters Whether to replace accented characters first. Default is FALSE
##' @inheritDotParams harmonize.x
##' @return Harmonized table
##'
##' @md
##' @import magrittr
##' @export
magerman_replace_umlaut <- function(x,
                                    x.umlaut.col = NULL,
                                    drop.umlaut.col = TRUE,
                                    replace.accented.characters = FALSE,
                                    ...) {
    ## get x vector...
    x.vector <- harmonize.x(x, ...)
    ## identify names with umlauts
    if (!is.null(x.umlaut.col) & !is.atomic(x)) {
        x.umlaut <- x[[x.umlaut.col]] %>% as.logical()
        ## drop x.umlaut.col
        if (isTRUE(drop.umlaut.col)) x[[x.umlaut.col]] <- NULL
    } else {
        replace.accented.characters <- TRUE
        x.umlaut <- x.vector %>%
            magerman_detect_umlaut(return.just.codes = TRUE) %>%
            as.logical()
    }
    ## replace accented characters
    if (replace.accented.characters) {
        x.vector %<>%
            magerman_replace_accented_characters
    }
    ## check if there are at least some umlauts
    if (any(as.logical(x.umlaut), na.rm = TRUE)) {
        ## transform umlaut
        x.harmonized <- x.vector %>%
            ## first "AE", "OE", "UE" -> "A", "O", "U"
            harmonize_replace(
                patterns = magerman.patterns.umlaut,
                patterns.col = 3,
                patterns.replacements.col = 2
            ) %>%
            ## then "A", "O", "U" -> "AE", "OE", "UE"
            harmonize_replace(
                patterns = magerman.patterns.umlaut,
                patterns.col = 2,
                patterns.replacements.col = 3
            )
        ## check which one match original umlaut
        x.harmonized.keep <-
            x.harmonized %in%
            x.harmonized[sapply(x.umlaut, isTRUE)]
        ## if does not match umlaut replace with original
        x.harmonized %<>%
            inset(
                !x.harmonized.keep,
                x.vector[!x.harmonized.keep]
            )
        ## return table
        harmonize.x(x, x.harmonized, ...) %>% return()
    } else {
        harmonize.x(x, x.vector, ...) %>% return()
    }
}

## Tests
## data.frame(
##   test = c("MÄKARÖNI ETÖ FKÜSNÖ Ltd"
##        , "MSLab CÖ."
##        , "MSLab Co."
##        , "MSLaeb Comp."
##        , "MSLab Comp."
##        , "ÃÄÅÆÇÈÉÌÍÏÐÑÒÖØÚÝŸ") %>% toupper
## , log = "lot of coffee"
## , umlaut = c(TRUE, TRUE, TRUE, FALSE, FALSE, TRUE)) %>%
##   magerman.replace.umlaut(return.x.cols.all = FALSE
##                         , x.umlaut.col = "umlaut"
##                         , drop.umlaut.col = TRUE)
## --------<<  Umlaut Harmonization:1 ends here



## -------->>  [[file:../harmonizer.src.org::*Combined Magerman Procedures][Combined Magerman Procedures:2]]
##' Harmonizes strings using exact procedures described in Magerman et al. 2009.
##' @param x table or vector
##' @param magerman.procedures Named list of procedures (closures) to apply to x. If we need to pass arguments to some of the procedures it can be done by specifying sub-list where the first element is procedure and the rest its arguments. Names of the list elements are used for progress messages. Procedures can also be passed as data.frame in which case it will be converted to list of procedures with `harmonize.make.procedures.list` (see its help for the correct format of data.frame with procedures). Default is `magerman.procedures.table`.
##' @param detect.legal.form Whether to detect legal forms. Default is FALSE
##' @param return.x.before.common.words.removal Whether to save harmonized column before `common.words.removal` procedure. Default is FALSE
##' @param return.x.cols.all Whether to return initial column in x. Default is FALSE
##' @inheritDotParams harmonize
##' @return Harmonized table
##'
##' @references Magerman et al., 2006 - Data Production Methods for Harmonized Patent Statistics: Patentee Name Harmonization
##'
##' @md
##' @import magrittr
##' @export
harmonize_magerman <- function(x,
                               magerman.procedures = magerman.procedures.table,
                               detect.legal.form = FALSE,
                               return.x.before.common.words.removal = FALSE,
                               return.x.cols.all = FALSE,
                               ...) {
    if (is.data.frame(magerman.procedures)) {
        magerman.procedures %<>% harmonize.make.procedures.list
    }
    ## do some tweaks on magerman.procedures
    if (!detect.legal.form) {
        magerman.procedures %<>%
            extract(sapply(., extract2, 1) %>%
                sapply(equals, "magerman.detect.legal.form") %>%
                not())
    }
    if (return.x.before.common.words.removal) {
        magerman.procedures %<>%
            inset2(
                sapply(., extract2, 1) %>%
                    sapply(equals, "magerman.remove.legal.form.and.clean") %>%
                    which(),
                list("magerman.remove.legal.form.and.clean", return.x.cols.all = TRUE)
            )
    }
    if (return.x.cols.all) {
        magerman.procedures %<>%
            inset2(1, c(as.list(extract2(., 1)), return.x.cols.all = TRUE))
    }
    harmonize(x, magerman.procedures, ...)
}
## --------<<  Combined Magerman Procedures:2 ends here


