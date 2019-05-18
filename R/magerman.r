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

magerman.proprietary.characters <- function(x, ...)
 {
     harmonize.replace(x, magerman.proprietary.characters, ...)
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
magerman.remove.special.characters <- function(x) {
  str_remove_all(x, "[^A-Z0-9\\-+'\"#*;@!?/&():;,. ]")
}

## test
## "LK \tD©𝍎 ၍\tF:'\";092834!@#$%^&*()_+-\n\t" %>% 
##   magerman.remove.special.characters %>%
##   message

magerman.remove.double.spaces <- function(x) {
    stri_replace_all(x, "\\s\\s", " ")
}

magerman.remove.double.quotation.marks.irregularities <- function(x) {
    x %>%
      stri_replace_all_regex("^\"\"\\s(.*)\"$", "\"\"$1\"") %>%
      stri_replace_all_regex("^\"(.*)\\s\"\"$", "\"$1\"\"")
}

## test
  ## c(
  ##   "\"\" asdlkjs  \"ad\" f\"\""
  ## , "\"\"asdlkjs \"\"ad\"\" f \"\""
  ##    ) %>% 
  ##    magerman.remove.double.quotation.marks.irregularities

magerman.remove.double.quotation.marks.beginning.end <- function(x) {
       stri_replace_all_regex(x, "^\"\"((?:(?!\"\").)*)\"\"$", "$1")
 }

 ## test
## c(
##   "\"\"asdlkjs  \"ad\" f\"\"" # delete quotes here
## , "\"\"asdlkjs \"\"ad\"\" f\"\"" # do not delete here
##    ) %>% 
##    magerman.remove.double.quotation.marks.beginning.end

magerman.remove.non.alphanumeric.at.the.beginning <- function(x) {
         stri_replace_all_regex(x, "^[^A-Z0-9\"@('#!*/]+", "")
   }

## Test:
## c("_MSLab Co."
## , "?MSLab Co."
## , ".-:MSLab Co.") %>% magerman.remove.non.alphanumeric.at.the.beginning


  magerman.remove.non.alphanumeric.at.the.end <- function(x) {
         stri_replace_all_regex(x, "[^A-Z0-9.'\")]+$", "")
   }

## Test:
## c("MSLab Co. :"
## , "MSLab Co.++"
## , "MSLab Co.*&^") %>% magerman.remove.non.alphanumeric.at.the.end

magerman.detect.legal.form <- function(x, ...) {
  harmonize.detect(x, magerman.patterns.legal.form, ...)
}


## Test
read.or.make.rds("magerman.patterns.legal.form", dir = "data", do.not.make = TRUE)

c("lksdjf MFG. CO, INC", "lksdjf MFG. CO, INC") %>%
  magerman.detect.legal.form %>% extract2(2)


magerman.patterns.legal.form %>% head
