## Harmonize Paten Assignees Names
## ================================================================================

## TODO: Add eurostat harmonization
## TODO: Add harmonization from Magerman, T., Van Looy, B., Song, X., European Commission, & Eurostat. (2006). Data production methods for harmonised patent statistics patentee name harmonisation. Luxembourg: Publications Office. Retrieved from http://epp.eurostat.ec.europa.eu/cache/ITY_OFFPUB/KS-AV-06-002/EN/KS-AV-06-002-EN.PDF
## TODO: Add legal form auto-recognition and country guess

## sequence used in NBER PDP
## ................................................................................
## - 0 leading and trimming spaces
## - 1 punctuation2
## - 2 derwent 
## - 3 standard_name 
## - 4 corporates
## - 5 (combabbrev) - it is just coding for legal form
## - 6 (stem_name) - I left common names untouched

## --------------------------------------------------------------------------------
## Load or Install Packages (for testing)
## --------------------------------------------------------------------------------
## for(pkg in c('pbapply'
##              , "stringi"
##            , 'stringr'
##            , 'data.table'
##            , 'dplyr'
##            , 'magrittr'
##            , "XML"
##              , "xml2"))
##     if(!require(pkg, character.only = TRUE)) {
##         install.packages(pkg, repos = 'http://cloud.r-project.org')
##         library(pkg, character.only = TRUE)
##     }
## --------------------------------------------------------------------------------

## Define functions for harmonization
## ================================================================================


## Convert HTML characters to UTF-8 (this one is 1/3 faster than htmlParse but it is still very slow)
## from - http://stackoverflow.com/questions/5060076/convert-html-character-entity-encoding-in-r?lq=1
#' @import xml2 magrittr 
html2txt <- function(strings) {
    sapply(strings, function(str) {
        if(!is.null(str)) {
            paste0("<x>", str, "</x>") %>%
                read_html %>%
                xml_text 
        } else {
            return(str)
        }
    }) %>% as.vector
}


## Encodes as UTF-8
#' @import stringr
toutf <- function(str) str_conv(str, "UTF-8")


## Translates non-ascii symbols to its ascii equivalent
#' @import stringr magrittr
toascii <- function(str) {
    ## utf <- "ŠŒŽšœžŸ¥µÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝßàáâãäåæçèéêëìíîïðñòóôõöøùúûüýÿ" %>% toutf
    utf <- system.file(package = "harmonizer") %>%
        file.path("enc/enc.txt") %>%
        readLines %>%
        str_conv("UTF-8")
    ascii <- "SOZsozYYuAAAAAAACEEEEIIIIDNOOOOOOUUUUYsaaaaaaaceeeeiiiionoooooouuuuyy"
    chartr(utf, ascii, str)
}


## Trims whitespases
#' @import stringr stringi
trims <- function(strings) {
    gsub("\\s+", " ", stringr::str_trim(strings))
}


## Clear content in brackets
#' @import stringr
clear.in.brackets <- function(str) str_replace_all(str, "<.*>|\\(.*\\)|\\{.*\\}|\\[.*\\]", "")


## Makes list of supstitutes given csv files with two columns (del, ins)
#' @import magrittr data.table
make.subs.table <- function(harm.files) {
    harm.files %>%
        lapply(function(file)
            read.csv(file
                   , header = FALSE
                   , col.names = c("del", "ins")
                   , as.is = TRUE
                   , colClasses = c("character", "character")
                   , na.strings = NULL)) %>%
        rbindlist
}

## Subsitutes strings
#' @import magrittr stringi
standardize <- function(org.names, subs.table, add.spaces = TRUE) {
    if(add.spaces) org.names <- paste(" ", org.names, " ")  # add spaces
    stri_replace_all_fixed(org.names, subs.table$del, subs.table$ins, vectorize_all = FALSE) %>% return
}

## Detects type of organization based on clues from 'harm.codes.list'
#' @import magrittr stringi
harm.detect <- function(org.names, harm.codes.list, add.spaces = TRUE){
    if(add.spaces) {
        org.names <- paste(" ", org.names, " ")  # add spaces
    }
    lapply(org.names, function(org)
        stri_detect_fixed(org, harm.codes.list$ins) %>%
        harm.codes.list$del[.] %>%
        ##head(n = 1) %>%
        str_c(collapse = "-") %>%  # for combinations
        ifelse(length(.) == 0, "unknown", .)) %>% unlist
}

## Testing
## harm.detect(
##     c("lala  UNIV LTD "
##     , "<br> asdf $ &AMP; &oacute; lala Ï a\n\u00b5\u00b5")
## , harm.codes.list)



## Load Substitution Rules
## ================================================================================

## Path for files with substitutions
harm.dir <- system.file(package = "harmonizer") %>%
    file.path("nber-pdp-harmonization")


## Substitutions
harm.subs.files <- c("nber-pdp-harmonization-(punctuation2_not_all-2,5,6,10).csv"
                   , "nber-pdp-harmonization-(standard_name-8).csv"
                   , "nber-pdp-harmonization-(nameonly_main-9).csv"
                   , "nber-pdp-harmonization-(derwent_standardisation_BHH-9).csv")

harm.subs.files.sansderwent <- c("nber-pdp-harmonization-(punctuation2_not_all-2,5,6,10).csv"
                   , "nber-pdp-harmonization-(standard_name-8).csv"
                   , "nber-pdp-harmonization-(nameonly_main-9).csv")

harm.subs.files.punctuation <- "nber-pdp-harmonization-(punctuation2_not_all-2,5,6,10).csv"

## Removals
harm.rem.files <- c("nber-pdp-harmonization-(stem_name-8).csv"
                    ## , "nber-pdp-harmonization-(non_corporates_univ-8).csv"
                    )

## Codes and clues for organization type
harm.codes.files <- c("nber-pdp-harmonization-(codes_org_type-9).csv")



## Wrapers for standardizes
## ================================================================================
#' @import magrittr
standardize.nber <- function(org.names) {
    file.path(harm.dir, c(harm.subs.files
                        , harm.rem.files)) %>% 
        make.subs.table %>%
        {standardize(org.names, .)}
}

#' @import magrittr
standardize.nber.sansremovals <- function(org.names) {
    file.path(harm.dir, harm.subs.files) %>% 
        make.subs.table %>%
        {standardize(org.names, .)}
}

#' @import magrittr
standardize.nber.sansderwent.sansremovals <- function(org.names) {
    file.path(harm.dir, harm.subs.files.sansderwent) %>% 
        make.subs.table %>%
        {standardize(org.names, .)}
}

#' @import magrittr
standardize.nber.sansderwent <- function(org.names) {
    file.path(harm.dir, c(harm.subs.files.sansderwent
                        , harm.rem.files)) %>% 
        make.subs.table %>%
        {standardize(org.names, .)}
}

#' @import magrittr
standardize.punctuation <- function(org.names) {
    file.path(harm.dir, harm.subs.files.punctuation) %>% 
        make.subs.table %>%
        {standardize(org.names, .)}
}









## This function combines all previous functions
#' @title Harmonizes organizational names. 
#'
#' @description
#' Returns harmonized version of organizational names.
#' @param org.names Character vector of organizational names to harmonize
#' @param procedures List of harmonization procedures. Each procedure can be specified as a string representing procedure name (see details for procedure names) or as a list where the first element should be procedure name (string) and other elements will passed as arguments to this procedure.
#' @return Character vector of harmonized  organizational names
#' @import magrittr pbapply stringr stringi data.table xml2
#' @export
#' @examples
#' org.names.test <- c("žŸong-ÂÃÇÈÏa\n\u00b5 &oacute;\u00b5<p>,  INt LTD &AMP; Co;  "
#'                   , "<br> the $ (&AMP; C&oacute;MP comPANY) Ïotta")
#' data.frame(original = org.names.test
#'          , harmonized = harmonize(org.names.test))
#' @details The following procedures are available:
#' * toutf - encode as UTF-8 (a wrapper for str_conv(str, "UTF-8"))
#' * tolower - lowercase
#' * toupper - uppercase
#' * html2txt - remove HTML symbols and tags (takes lot of time)
#' * toascii - replace accented characters with ascii equivalent
#' * clear.in.brackets - removes brackets and its content - (), [], {}, <>
#' * trims - removes double white spaces and trims white spaces
#' * standardize.nber - applies standard name substitutions following NBER's PDP (Thoma, et al, 2010) - https://sites.google.com/site/patentdataproject/Home/posts/namestandardizationroutinesuploaded
#' * standardize.nber.sansremovals - same as standardize.nber but without removals of organization legal form
#' * standardize.nber.sansderwent - same as standardize.nber but without Derwent names standardization
#' * standardize.nber.sansderwent.sansremovals - same as standardize.nber.sansderwent but without removals of organization legal form
#' * standardize.punctuation - removes punctuation
#' @md
harmonize <- function(org.names
                    , procedures = list(
                          "toutf"
                        , "tolower"
                        , "html2txt"
                        , "clear.in.brackets"
                        , "toascii"
                        , "toupper"
                        , "standardize.nber"
                        , "trims"
                      )) {
    ## Apply harmonization
    if(is.character(org.names)) {
        for(procedure in procedures) {
            print(paste("Running procedure:", procedure[[1]]))
            org.names %<>% 
                list %>%
                c(procedure[-1]) %>%        # add arguents
                {do.call(procedure[[1]], .)}
        }
        return(org.names)
    } else {
        return(org.names)
    }
}



## Test
## org.names.test <- c("žŸong-ÂÃÇÈÏa\n\u00b5 &oacute;\u00b5<p>,  INt LTD &AMP; Co;  "
##                   , "<br> the $ (&AMP; C&oacute;MP comPANY) Ïotta")
## data.frame(original = org.names.test
##          , harmonized = harmonize(org.names.test))


 

