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

magerman.remove.html.codes <- function(x, ...) {
  harmonize.replace(x
                  , patterns = "<BR>"
                  , replacements = " "
                  , ...)
}

## ## Tests
## "bla bla <BR>" %>% magerman.remove.html.codes

## this should accept both vector and table and return eather vector or a table
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

magerman.replace.proprietary.characters <- function(x, ...)
 {
     harmonize.replace(x, magerman.patterns.proprietary.characters, ...)
 }

## test
## "&AMP;&OACUTE;&SECT; {UMLAUT OVER (E)} sdlfkjhhhh ;laskdjf&EXCL;" %>%
##   magerman.replace.proprietary.characters

## Assumes that all cahracters are in caps
magerman.replace.accented.characters <- function(x, ...)
 {
     harmonize.replace(x, magerman.patterns.accented.characters, ...)
 }

## Test
## "ŠŒŽšœžŸ¥µÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝßàáâãäåæçèéêëìíîïðñòóôõöøùúûüýÿ" %>%
##   magerman.replace.accented.characters

## somewhat works:
## [1] "ŠŒŽšœžY¥µAAAAAAAECEEEEIIIIÐNOOOOOØUUUUYßàáâãäåæçèéêëìíîïðñòóôõöøùúûüýÿ"

## Remove everything that is not:
## A-Z; 0-9; “-“; “+”; “’”; “””; “#”; “*”;“@”; “!”; “?”; “/”; “&”; “(“; “)”; “:”; “;”; “,”; “.”; “ “

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

magerman.detect.comma.period.irregularities <- function(x, ...)
{
  harmonize.replace(x 
                  , patterns =  
                      c(",([^\\s])" , "Patterns with comma not followed by space"
                      , "\\s," , "Patterns with comma preceded by space"
                      , "([^A-Za-z0-9])\\."  , "Patterns with period not preceded by a letter or digit") %>% 
                      matrix(byrow = TRUE, ncol = 2) %>%
                      data.frame
                  , patterns.type = "regex"
                  , codes.name = "comma.period.irregularities.candidates"
                  , ...)
}

magerman.replace.comma.period.irregularities.all <- function(x, ...)
{
  harmonize.replace(x 
                  , patterns =  
                      c(",([^\\s])"        , ", $1" 
                      , "\\s,"             , ","
                      , "([^A-Za-z0-9])\\.", "$1") %>%
                      matrix(byrow = TRUE, ncol = 2) %>%
                      data.frame
                  , patterns.type = "regex"
                  , ...)
}

## Test magerman.replace.comma.period.irregularities.all
## "A sentence with .irregular punctuation ,like commas , and periods ." %>% 
##  magerman.replace.comma.period.irregularities.all

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

magerman.detect.legal.form.end <- function(x, ...) {
  harmonize.detect(x
                 , magerman.patterns.legal.form.end
                 , patterns.codes.col = 3
                 , patterns.type = "ends"
                 , codes.first = TRUE
                 , ...)
}

magerman.replace.legal.form.end <- function(x, ...) {
  harmonize.replace(x
                  , patterns = magerman.patterns.legal.form.end
                  , patterns.type = "ends"
                  , ...)
}




magerman.detect.legal.form.beginning <- function(x, ...) {
    harmonize.detect(x
                   , patterns = data.table(pattern = "KABUSHIKI KAISHA"
                                         , legal.form = "KAISHA")
                   , patterns.type = "begins"
                   , codes.first = TRUE
                   , ...)
}

magerman.replace.legal.form.beginning <- function(x, ...) {
  harmonize.replace(x
                  , patterns = "KABUSHIKI KAISHA"
                  , patterns.type = "begins"
                  , ...)
}



magerman.detect.legal.form.middle <- function(x, ...) {
  harmonize.detect(x
                 , magerman.patterns.legal.form.middle
                 , patterns.codes.col = 3
                 , patterns.type = "fixed"
                 , codes.first = TRUE
                 , ...)
}

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

magerman.detect.legal.form <- function(x) {
  x %>% 
    magerman.detect.legal.form.end %>% 
    magerman.detect.legal.form.beginning(x.codes.add = TRUE) %>% 
    magerman.detect.legal.form.middle(x.codes.add = TRUE)
}

## Test
## c("lksdjf MFG. GMBH CO, INC"
##  , "MSlab Co."
##  , "IBM Corp."
##  , "MSlab Co. GMBH & CO.KG lalal"
##  , "KABUSHIKI KAISHA MSlab Co. ") %>% 
##    magerman.detect.legal.form

magerman.remove.legal.form <- function(x) {
  x %>%
    magerman.detect.legal.form.end(codes.name = "not.to.replace") %>%
    magerman.replace.legal.form.end %>%
    magerman.replace.legal.form.beginning(
      x.rows = harmonize.is.empty(.[[length(.)]])
    , harmonized.omitted.col = 1
    , harmonized.name = names(.)[1]) %>% 
    magerman.replace.legal.form.middle(
      x.rows = harmonize.is.empty(.[[length(.)]])
    , harmonized.omitted.col = 1
    , harmonized.name = names(.)[1]
      ## drop last col "not.to.replace"
    , return.x.cols = -c(1, length(.)))
    }

magerman.remove.legal.form.and.clean <- function(x) {
  x %>%
    magerman.remove.legal.form %>% 
    harmonize.replace(
      patterns = c("[-;:,&]*\\s*$", "^\\s*")
    , patterns.type = "regex"
    , harmonized.name = names(.)[1])
}




## Test
## c("lksdjf MFG. GMBH CO,; INC"
## , "MSlab Co."
## , "IBM Corp."
## , " MSlab Co. GMBH & CO.KG lalal  "
## , "KABUSHIKI KAISHA MSlab Co.") %>%
##   toupper %>% 
##   magerman.remove.legal.form.clean


## data.table(c("lksdjf MFG. GMBH CO,; INC"
##            , "MSlab Co."
##            , "IBM Corp."
##            , " MSlab Co. GMBH & CO.KG lalal  "
##            , "KABUSHIKI KAISHA MSlab Co.") %>% toupper
##          , somevar = c(1,2,3,4,5)) %>%
##   magerman.remove.legal.form.clean

magerman.remove.common.words.at.the.end  <- function(x, ...) {
magerman.patterns.common.words.at.the.end %>%
  harmonize.replace(x
                 , patterns = .
                 , patterns.type = "ends"
                 , ...)
}

magerman.remove.common.words.at.the.beginning  <- function(x, ...) {
magerman.patterns.common.words.at.the.beginning %>%
  harmonize.replace(x
                 , patterns = .
                 , patterns.type = "begins"
                 , ...)
}



magerman.remove.common.words.anywhere  <- function(x, ...) {
magerman.patterns.common.words.anywhere %>%
  harmonize.replace(x
                 , patterns = .
                 , patterns.type = "fixed"
                 , ...)
}


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

magerman.replace.spelling.variation <- function(x, ...) {
  magerman.patterns.spelling.variation %>%
    harmonize.replace(x, patterns = ., ...)
}

## Test
## c("CHEMICALS SYSTEMEN MSlab Ltd."
## , "ELECTRONICS SYSTEMES MSlab Co.") %>%
##   magerman.replace.spelling.variation(return.x.cols.all = TRUE)

magerman.condence <- function(x, ...) {
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
##   harmonize.magerman.condence(return.x.cols.all = TRUE)

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


magerman.replace.umlaut <- function(x
                                  , x.umlaut.col = NULL
                                  , replace.accented.characters = FALSE
                                  , ...) {
  ## get x vector...
  x.vector <- harmonize.x(x, ...)
  ## identify names with umlauts
  x.umlaut <- if(!is.null(x.umlaut.col) & !is.atomic(x)) {
                x[[x.umlaut.col]]
              } else {
                replace.accented.characters <- TRUE
                magerman.detect.umlaut(x.vector, return.just.codes = TRUE)
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
      harmonize.replace(patterns = magerman.patterns.umlaut
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
##   lt = c("MÄKARÖNI ETÖ FKÜSNÖ Ltd"
##        , "MSLab CÖ."
##        , "MSLab Co."
##        , "MSLaeb Comp."
##        , "MSLab Comp."
##        , "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝŸ") %>% toupper
## , log = "lalala") %>%
##   magerman.replace.umlaut(return.x.cols.all = FALSE)

magerman.procedures.list <- list(
  ## preprocessing
  "harmonize.toupper"
, "harmonize.squish.spaces"
  ## characters
, "magerman.remove.html.codes"
, "harmonize.squish.spaces"
, "magerman.replace.sgml.characters"
, "magerman.replace.proprietary.characters"
# code names with umlaut for umlaut harmonization
, list("magerman.detect.umlaut"
     , codes.name = "magerman.umlaut")
, "magerman.replace.accented.characters"
  ## punctuation
, "magerman.remove.special.characters"
, "magerman.remove.double.quotation.marks.irregularities"
, "magerman.remove.double.quotation.marks.beginning.end"
, "magerman.remove.non.alphanumeric.at.the.beginning"
, "magerman.remove.non.alphanumeric.at.the.end"
, "magerman.replace.comma.period.irregularities"
  ## legal form
, "magerman.detect.legal.form"
, "magerman.remove.legal.form.and.clean"
  ## common words
, "magerman.remove.common.words"
  ## spelling variation
, "magerman.replace.spelling.variation"
  ## condensing
, "magerman.condence"
  ## umlaut harmonization
, list("magerman.replace.umlaut"
     , x.umlaut.col = "magerman.umlaut"
       ## this function wont work properly for batches
       ## it need to look up matches in the whole corpus
     , progress = FALSE)
)


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


## ok this returns a lot of stuff
## I want default behavior (just include x)


## Test
## data.table(name = c("MÄKARÖNI ETÖ FKÜSNÖ Ltd"
##                   , "MSLab CÖ. <a href=lsdldf> <br> <\\a>"
##                   , "MSLab Co."
##                   , "MSLaeb Comp."
##                   , "MSLab Comp."
##                   , "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝŸ") %>%
##              rep(10)
##          , foo = "lalala" ) %>% 
##   harmonize.magerman(progress.min = 10)
