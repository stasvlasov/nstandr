##' Detects candidates for characters that need to be cleaned
##' @param x table
##' @inheritDotParams harmonize.detect
##' @return coded table
##' 
##' @md 
##' @import magrittr
##' @export
magerman.detect.characters <- function(x, ...) {
  harmonize.detect(x
                 , patterns = c( "\\{.+\\}" , "propriety coded characters {xxx}"
                              , "\\[0.+\\]" , "propriety coded characters [0xxx]"
                              , "\\(.+\\)"  , "propriety coded characters (xxx)"
                              , "&.+;"      , "sgml coded characters"
                              , "<.+>"      , "html coded characters") %>%
                     matrix(byrow = TRUE, ncol = 2) %>%
                     data.frame
                 , patterns.type = "regex"
                 , codes.name = "characters.cleaning.candidates"
                 , ...)
}

## Test
## "Chip &AMP; Dayle (lala) [0x2345] {abs} ops html <br>" %>% 
##   magerman.detect.characters

##' Removes html codes
##' @param x table
##' @inheritDotParams harmonize.replace
##' @return harmonized table
##' 
##' @md 
##' @export
magerman.remove.html.codes <- function(x, ...) {
  harmonize.replace(x
                  , patterns = "<BR>"
                  , replacements = " "
                  , ...)
}

## ## Tests
## "bla bla <BR>" %>% magerman.remove.html.codes

##' Replaces sgml characters. Accept both vector and table and return either vector or a table
##' @param x table 
##' @inheritDotParams harmonize.replace
##' @return Harmonized table
##' 
##' @md 
##' @export 
magerman.replace.sgml.characters <- function(x, ...)
 {
     harmonize.replace(x, magerman.patterns.sgml.characters, ...)
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

##' Replaces proprietary characters
##' @param x table 
##' @inheritDotParams harmonize.replace
##' @return Harmonized table
##' 
##' @md 
##' @export 
magerman.replace.proprietary.characters <- function(x, ...)
 {
     harmonize.replace(x, magerman.patterns.proprietary.characters, ...)
 }

## test
## "&AMP;&OACUTE;&SECT; {UMLAUT OVER (E)} sdlfkjhhhh ;laskdjf&EXCL;" %>%
##   magerman.replace.proprietary.characters

##' Replaces accented characters
##'
##' Assumes that all characters are in caps.
##' @param x table
##' @inheritDotParams harmonize.replace
##' @return Harmonized table
##' 
##' @md 
##' @export 
magerman.replace.accented.characters <- function(x, ...)
 {
     harmonize.replace(x, magerman.patterns.accented.characters, ...)
 }

## Test
## "ŠŒŽšœžŸ¥µÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝßàáâãäåæçèéêëìíîïðñòóôõöøùúûüýÿ" %>%
##   magerman.replace.accented.characters

## somewhat works:
## [1] "ŠŒŽšœžY¥µAAAAAAAECEEEEIIIIÐNOOOOOØUUUUYßàáâãäåæçèéêëìíîïðñòóôõöøùúûüýÿ"

##' Removes special characters. I.e., everything that is not:
## A-Z; 0-9; “-“; “+”; “’”; “””; “#”; “*”;“@”; “!”; “?”; “/”; “&”; “(“; “)”; “:”; “;”; “,”; “.”; “ “
##' @param x table
##' @inheritDotParams harmonize.replace
##' @return Harmonized table
##' 
##' @md 
##' @export 
magerman.remove.special.characters <- function(x, ...) {
  harmonize.replace(x
                  , "[^A-Z0-9\\-+'\"#*;@!?/&():;,. ]"
                  , patterns.type = "regex"
                  , ...)
}

## test
## "LK \tD©𝍎 ၍\tF:'\";092834!@#$%^&*()_+-\n\t" %>% 
##   magerman.remove.special.characters %>%
##   message

##' Removes double spaces
##' @param x table
##' @inheritDotParams harmonize.replace
##' @return Harmonized table
##' 
##' @md 
##' @export 
magerman.remove.double.spaces <- function(x, ...) {
    harmonize.replace(x
                    , "\\s+"
                    , replacements = " "
                    , patterns.type = "regex"
                    , ...)
}


## Test magerman.remove.double.spaces
## "  a   string with   many      douple    spaces      " %>% 
##   magerman.remove.double.spaces

##' Removes double quotation irregularities
##' @param x table
##' @inheritDotParams harmonize.replace
##' @return Harmonized table
##' 
##' @md 
##' @export 
magerman.remove.double.quotation.marks.irregularities <- function(x, ...) {
  harmonize.replace(x
                  , patterns = c("^\"\"\\s(.*)\"$", "^\"(.*)\\s\"\"$")
                  , replacements = c("\"\"$1\"", "\"$1\"\"")
                  , patterns.type = "regex"
                  , ...)
}

## Test   magerman.remove.double.quotation.marks.irregularities
## c("\"\" Merry  \"Cristmas\" Love\"\""
## , "\"\"Merry \"\"Cristmas\"\" Love \"\"") %>%
##   magerman.remove.double.quotation.marks.irregularities(bind.x.cols = "all")

##' Removes double quotation irregularities
##' @param x table
##' @inheritDotParams harmonize.replace
##' @return Harmonized table
##' 
##' @md 
##' @export 
magerman.remove.double.quotation.marks.beginning.end <- function(x, ...) {
  harmonize.replace(x
                  , patterns = "^\"\"((?:(?!\"\").)*)\"\"$"
                  , replacements = "$1"
                  , patterns.type = "regex"
                  , ...)
}

## Test magerman.remove.double.quotation.marks.beginning.end
## c("\"\"Merry  \"Cristmas\" Love\"\"" # delete quotes here
## , "\"\"Merry \"\"Cristmas\"\" Love\"\""  # do not delete here
##   ) %>%
##   magerman.remove.double.quotation.marks.beginning.end(bind.x.cols = "all")

##' Removes non alphanumeric characters
##' @param x table
##' @inheritDotParams harmonize.replace
##' @return Harmonized table
##' 
##' @md 
##' @export 
magerman.remove.non.alphanumeric.at.the.beginning <- function(x, ...) {
  harmonize.replace(x
                  , patterns = "^[^A-Z0-9\"@('#!*/]+"
                  , patterns.type = "regex"
                  , ...)
}

## Test:
## c("_MSLab Co."
## , "?MSLab Co."
## , ".-:MSLab Co.") %>% magerman.remove.non.alphanumeric.at.the.beginning


##' Removes non alphanumeric characters
##' @param x table
##' @inheritDotParams harmonize.replace
##' @return Harmonized table
##' 
##' @md 
##' @export 
magerman.remove.non.alphanumeric.at.the.end <- function(x, ...) {
  harmonize.replace(x
                  , patterns = "[^A-Z0-9.'\")]+$"
                  , patterns.type = "regex"
                  , ...)
}

## Test:
## c("MSLab Co. :"
## , "MSLab Co.++"
## , "MSLab Co.*&^") %>% magerman.remove.non.alphanumeric.at.the.end

##' Detects comma period irregularities
##' @param x table
##' @inheritDotParams harmonize.detect
##' @return Harmonized table
##' 
##' @md 
##' @import magrittr
##' @export 
magerman.detect.comma.period.irregularities <- function(x, ...)
{
  c(",([^\\s])", "Patterns with comma not followed by space"
  , "\\s,", "Patterns with comma preceded by space"
  , "([^A-Za-z0-9])\\.", "Patterns with period not preceded by a letter or digit") %>% 
    matrix(byrow = TRUE, ncol = 2) %>%
    data.frame %>% 
  harmonize.detect(x 
                  , patterns = .
                  , patterns.type = "regex"
                  , codes.name = "comma.period.irregularities.candidates"
                  , ...)
}

##' Replaces comma period irregularities
##' @param x table
##' @inheritDotParams harmonize.replace
##' @return Harmonized table
##' 
##' @md 
##' @import magrittr
##' @export 
magerman.replace.comma.period.irregularities.all <- function(x, ...)
{
  c(",([^\\s])"        , ", $1" 
  , "\\s,"             , ","
  , "([^A-Za-z0-9])\\.", "$1") %>%
    matrix(byrow = TRUE, ncol = 2) %>%
    data.frame %>% 
  harmonize.replace(x 
                  , patterns =  .
                  , patterns.type = "regex"
                  , ...)
}

## Test magerman.replace.comma.period.irregularities.all
## "A sentence with .irregular punctuation ,like commas , and periods ." %>% 
##  magerman.replace.comma.period.irregularities.all

##' Replaces comma period irregularities
##' @param x object
##' @inheritDotParams harmonize.replace
##' @return Harmonized table
##' 
##' @md 
##' @import magrittr
##' @export 
magerman.replace.comma.period.irregularities <- function(x, ...)
 {
   list(magerman.patterns.comma.followed.by.space
      , magerman.patterns.comma.preceded.by.space
      , magerman.patterns.periods) %>% 
     rbindlist %>% 
     harmonize.replace(x, ., patterns.type = 3, ...)
 }

## Test magerman.replace.comma.period.irregularities
## c("MSlab ,INC. ,LTD"
## , "MSlab ,LTD Universe") %>% 
##   magerman.replace.comma.period.irregularities(bind.x.cols = "all")

##' Detects legal form
##' @param x table
##' @inheritDotParams harmonize.detect
##' @return Harmonized table
##' 
##' @md 
##' @export 
magerman.detect.legal.form.end <- function(x, ...) {
  harmonize.detect(x
                 , magerman.patterns.legal.form.end
                 , patterns.codes.col = 3
                 , patterns.type = "ends"
                 , codes.first = TRUE
                 , ...)
}

##' Replaces legal form
##' @param x table
##' @inheritDotParams harmonize.replace
##' @return Harmonized table
##' 
##' @md 
##' @export 
magerman.replace.legal.form.end <- function(x, ...) {
  harmonize.replace(x
                  , patterns = magerman.patterns.legal.form.end
                  , patterns.type = "ends"
                  , ...)
}

##' Detects legal form
##' @param x table
##' @inheritDotParams harmonize.detect
##' @return Harmonized table
##' 
##' @md 
##' @export 
magerman.detect.legal.form.beginning <- function(x, ...) {
    harmonize.detect(x
                   , patterns = data.table(pattern = "KABUSHIKI KAISHA"
                                         , legal.form = "KAISHA")
                   , patterns.type = "begins"
                   , codes.first = TRUE
                   , ...)
}

##' Replaces legal form
##' @param x table
##' @inheritDotParams harmonize.replace
##' @return Harmonized table
##' 
##' @md 
##' @export 
magerman.replace.legal.form.beginning <- function(x, ...) {
  harmonize.replace(x
                  , patterns = "KABUSHIKI KAISHA"
                  , patterns.type = "begins"
                  , ...)
}



##' Detects legal form
##' @param x table
##' @inheritDotParams harmonize.detect
##' @return Harmonized table
##' 
##' @md 
##' @export 
magerman.detect.legal.form.middle <- function(x, ...) {
  harmonize.detect(x
                 , magerman.patterns.legal.form.middle
                 , patterns.codes.col = 3
                 , patterns.type = "fixed"
                 , codes.first = TRUE
                 , ...)
}

##' Replaces legal form
##' @param x table
##' @inheritDotParams harmonize.replace
##' @return Harmonized table
##' 
##' @md 
##' @export 
magerman.replace.legal.form.middle <- function(x, ...) {
  harmonize.replace(x
                  , patterns = magerman.patterns.legal.form.middle
                  , patterns.type = "fixed"
                  , ...)
}


## Test
## c("lksdjf MFG. CO, INC"
## , "MSlab Co."
## , "IBM Corp."
## , "MSlab Co. GMBH & CO.KG lalal"
## , "KABUSHIKI KAISHA MSlab Co. ") %>%
##   toupper %>% 
##   magerman.detect.legal.form.end

##' Detects legal form
##' @param x table
##' @inheritDotParams magerman.detect.legal.form.end
##' @return Harmonized table
##' 
##' @md
##' @import magrittr
##' @export 
magerman.detect.legal.form <- function(x, ...) {
  x %>% 
    magerman.detect.legal.form.end(...) %>% 
    magerman.detect.legal.form.beginning(x.codes.col = ncol(.)
                                       , x.codes.update.empty = TRUE) %>% 
    magerman.detect.legal.form.middle(x.codes.col = ncol(.)
                                    , x.codes.update.empty = TRUE)
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
magerman.remove.legal.form <- function(x) {
  x %>%
    magerman.detect.legal.form.end(codes.name = "not.to.replace") %>%
    magerman.replace.legal.form.end %>%
    magerman.replace.legal.form.beginning(
      x.rows = harmonize.is.empty(.[[ncol(.)]])
    , x.harmonized.col = 1) %>% 
    magerman.replace.legal.form.middle(
      x.rows = harmonize.is.empty(.[[ncol(.)]])
    , x.harmonized.col = 1
      ## drop last col "not.to.replace"
    , return.x.cols = -ncol(.))
    }



##' Removes legal form
##' @param x table
##' @return Harmonized table
##' 
##' @md 
##' @import magrittr
##' @export 
magerman.remove.legal.form.and.clean <- function(x) {
  x %>%
    magerman.remove.legal.form %>% 
    harmonize.replace(
      patterns = c("[-;:,&]*\\s*$", "^\\s*")
    , patterns.type = "regex"
    , harmonized.name = names(.)[1])
}






## ## Test
## c("lksdjf MFG. GMBH CO,; INC"
## , "MSlab Co."
## , "IBM Corp."
## , " MSlab Co. GMBH & CO.KG lalal  "
## , "KABUSHIKI KAISHA MSlab Co.") %>%
##   toupper %>% 
##   magerman.remove.legal.form.and.clean


## data.table(c("lksdjf MFG. GMBH CO,; INC"
##            , "MSlab Co."
##            , "IBM Corp."
##            , " MSlab Co. GMBH & CO.KG lalal  "
##            , "KABUSHIKI KAISHA MSlab Co.") %>% toupper
##          , somevar = c(1,2,3,4,5)) %>%
##   magerman.remove.legal.form.clean

##' Removes common words
##' @param x table
##' @inheritDotParams harmonize.replace
##' @return Harmonized table
##' 
##' @md 
##' @export 
magerman.remove.common.words.at.the.end  <- function(x, ...) {
  harmonize.replace(x
                 , patterns = magerman.patterns.common.words.at.the.end
                 , patterns.type = "ends"
                 , ...)
}

##' Removes common words
##' @param x table
##' @inheritDotParams harmonize.replace
##' @return Harmonized table
##' 
##' @md 
##' @export 
magerman.remove.common.words.at.the.beginning  <- function(x, ...) {
  harmonize.replace(x
                 , patterns = magerman.patterns.common.words.at.the.beginning
                 , patterns.type = "begins"
                 , ...)
}



##' Removes common words
##' @param x table
##' @inheritDotParams harmonize.replace
##' @return Harmonized table
##' 
##' @md 
##' @export 
magerman.remove.common.words.anywhere  <- function(x, ...) {
  harmonize.replace(x
                 , patterns = magerman.patterns.common.words.anywhere
                 , patterns.type = "fixed"
                 , ...)
}


##' Removes common words
##' @param x table
##' @inheritDotParams harmonize.replace
##' @return Harmonized table
##' 
##' @md 
##' @import magrittr
##' @export 
magerman.remove.common.words  <- function(x, ...) {
  x %>% 
    magerman.remove.common.words.at.the.end(...) %>%
    magerman.remove.common.words.at.the.beginning(...) %>%
    magerman.remove.common.words.anywhere(...)
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

##' Replaces spelling variation
##' @param x table 
##' @inheritDotParams harmonize.replace
##' @return Harmonized table
##' 
##' @md 
##' @export 
magerman.replace.spelling.variation <- function(x, ...) {
  harmonize.replace(x
                  , patterns = magerman.patterns.spelling.variation
                  , ...)
}

## Test
## c("CHEMICALS SYSTEMEN MSlab Ltd."
## , "ELECTRONICS SYSTEMES MSlab Co.") %>%
##   magerman.replace.spelling.variation(return.x.cols.all = TRUE)

##' Condenses string
##' @param x table
##' @inheritDotParams harmonize.replace
##' @return Harmonized table
##' 
##' @md 
##' @export 
magerman.condense <- function(x, ...) {
  harmonize.replace(x
                  , patterns = "[^a-zA-Z0-9]+"
                  , patterns.type = "regex"
                  , ...)
}

## Test
## c("lksdjf MFG. GMBH CO,; INC"
## , "MSlab Co."
## , "IBM Corp."
## , " MSlab Co. GMBH & CO.KG lalal  "
## , "KABUSHIKI KAISHA MSlab Co.") %>%
##   magerman.condence(return.x.cols.all = TRUE)

##' Detects umlauts
##' @param x table
##' @inheritDotParams harmonize.detect
##' @return Coded table
##' 
##' @md 
##' @export
magerman.detect.umlaut <- function(x, ...) {
  harmonize.detect(x, 
                 , patterns = magerman.patterns.umlaut
                 , patterns.codes.col = 4
                 , patterns.type = "fixed"
                 , codes.first = TRUE
                 , ...)
}



## Test
## c("MÄKARÖNI ETÖ FKÜSNÖ Ltd"
## , "MSLab Co."
## , "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝŸ") %>%
##   magerman.detect.umlaut(return.just.codes = FALSE)


##' Replaces Umlauts
##' @param x 
##' @param x.umlaut.col Column with logical values indicating weather a corresponding string has an umlaut. Default is NULL so it detects is automatically first
##' @param drop.umlaut.col Whether to drop `umlaut.col`. Default is FALSE
##' @param replace.accented.characters Whether to replace accented characters first. Default is FALSE
##' @inheritDotParams harmonize.x
##' @return Harmonized table
##' 
##' @md 
##' @import magrittr
##' @export 
magerman.replace.umlaut <- function(x
                                  , x.umlaut.col = NULL
                                  , drop.umlaut.col = FALSE
                                  , replace.accented.characters = FALSE
                                  , ...) {
  ## get x vector...
  x.vector <- harmonize.x(x, ...)
  ## identify names with umlauts
  if(!is.null(x.umlaut.col) & !is.atomic(x)) {
    x.umlaut <- x[[x.umlaut.col]] %>% as.logical
    ## drop x.umlaut.col
    if(isTRUE(drop.umlaut.col)) x[[x.umlaut.col]] <-  NULL
  } else {
    replace.accented.characters <- TRUE
    x.umlaut <- x.vector %>% 
      magerman.detect.umlaut(return.just.codes = TRUE) %>%
      as.logical
  }
  ## replace accented characters
  if(replace.accented.characters) {
    x.vector %<>%
      magerman.replace.accented.characters
  }
  ## check if there are at least some umlauts
  if(any(as.logical(x.umlaut), na.rm = TRUE)) {
    ## transform umlaut
    x.harmonized <- x.vector %>%
      ## first "AE", "OE", "UE" -> "A", "O", "U"
      harmonileze.replace(patterns = magerman.patterns.umlaut
                      , patterns.col = 3
                      , patterns.replacements.col = 2) %>% 
      ## then "A", "O", "U" -> "AE", "OE", "UE"
      harmonize.replace(patterns = magerman.patterns.umlaut
                      , patterns.col = 2
                      , patterns.replacements.col = 3)
    ## check which one match original umlaut
    x.harmonized.keep <-
      x.harmonized %in%
      x.harmonized[sapply(x.umlaut, isTRUE)]
    ## if does not match umlaut replace with original
    x.harmonized %<>%
      inset(!x.harmonized.keep
          , x.vector[!x.harmonized.keep])
    ## return table
    harmonize.x(x ,x.harmonized, ...) %>% return()
  } else {
    harmonize.x(x ,x.vector, ...) %>% return()
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

magerman.procedures.list <- list(
  ## prepossessing
  "Upper casing" = "harmonize.toupper"
, "Cleaning spaces" = "harmonize.squish.spaces"
  ## characters
, "Removing HTML codes" = "magerman.remove.html.codes"
, "Cleaning spaces (2)" = "harmonize.squish.spaces"
, "Replacing SGML coded characters" = "magerman.replace.sgml.characters"
, "Replacing proprietary characters" = "magerman.replace.proprietary.characters"
# code names with umlaut for umlaut harmonization
, "Detecting Umlauts" = list("magerman.detect.umlaut"
                           , codes.name = "magerman.umlaut")
, "Replacing accented characters" = "magerman.replace.accented.characters"
  ## punctuation
, "Removing special characters" = "magerman.remove.special.characters"
, "Fixing quotation irregularities" =  "magerman.remove.double.quotation.marks.irregularities"
, "Removing double quotations" = "magerman.remove.double.quotation.marks.beginning.end"
, "Removing non alphanumeric characters (1)" = "magerman.remove.non.alphanumeric.at.the.beginning"
, "Removing non alphanumeric characters (2)" = "magerman.remove.non.alphanumeric.at.the.end"
, "Fixing comma and period irregularities" = "magerman.replace.comma.period.irregularities"
  ## legal form
, "Detecting legal form" = list("magerman.detect.legal.form"
                              , codes.name = "legal.form")
, "Removing legal form" = "magerman.remove.legal.form.and.clean"
  ## common words
, "Removing common words" = "magerman.remove.common.words"
  ## spelling variation
, "Fixing spelling variations" = "magerman.replace.spelling.variation"
  ## condensing
, "Condensing" = "magerman.condense"
  ## umlaut harmonization
, "Fixing umlaut variations" = list("magerman.replace.umlaut"
     , x.umlaut.col = "magerman.umlaut"
     , drop.umlaut.col = TRUE
       ## this function wont work properly for batches
       ## it need to look up matches in the whole corpus
     , progress = FALSE)
)

##' Harmonizes strings using exact procedures described in Magerman et al. 2009.
##' @param x table or vector
##' @param magerman.procedures list of procedures to pass to `harmonize` function. Default is `magerman.procedures.list`
##' @param detect.legal.form Whether to detect legal forms
##' @param return.x.before.common.words.removal Whether to save harmonized column before `common.words.removal` procedure
##' @param return.x.cols.all Whether to return initial column in x
##' @inheritDotParams harmonize
##' @return Harmonized table
##'
##' @references Magerman et al., 2006 - Data Production Methods for Harmonized Patent Statistics: Patentee Name Harmonization
##' 
##' @md 
##' @import magrittr
##' @export 
harmonize.magerman <- function(x
                             , magerman.procedures = magerman.procedures.list
                             , detect.legal.form = FALSE
                             , return.x.before.common.words.removal = FALSE
                             , return.x.cols.all = FALSE
                             , ... ) {
  ## do some tweaks on magerman.procedures
  if(!detect.legal.form) {
    magerman.procedures %<>%
      extract(sapply(., extract2, 1) %>%
              sapply(equals, "magerman.detect.legal.form") %>%
              not)
  }
  if(return.x.before.common.words.removal) {
    magerman.procedures %<>%
      inset2(sapply(., extract2, 1) %>%
             sapply(equals, "magerman.remove.legal.form.and.clean") %>%
             which
           , list("magerman.remove.legal.form.and.clean", return.x.cols.all = TRUE))
  }
  if(return.x.cols.all) {
    magerman.procedures %<>% 
      inset2(1, c(as.list(extract2(.,1)), return.x.cols.all = TRUE))
  }
  harmonize(x, magerman.procedures, ...)
}


## Test with return magerman.procedures in harmonize.magerman
## harmonize.magerman()
## harmonize.magerman(detect.legal.form = TRUE)
## harmonize.magerman(return.x.before.common.words.removal = TRUE)
## harmonize.magerman(return.x.cols.all = TRUE)

## Test
## data.table(name = c("MÄKARÖNI ETÖ FKÜSNÖ Ltd"
##                   , "MSLab CÖ. <a href=lsdldf> <br> <\\a>"
##                   , "MSLab Co."
##                   , "MSLaeb Comp."
##                   , "MSLab Comp. Ltd."
##                   , "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝŸ") %>% rep(200)
##          , foo = "I love coffee" ) %>% 
##   harmonize.magerman(progress.min = 50
##                    , detect.legal.form = TRUE)
