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
##            , "xml2"
##            , "readr"))
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
#' @import stringi stringr magrittr
toascii <- function(str, enc.detect = FALSE) {
    ## utf <- "ŠŒŽšœžŸ¥µÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝßàáâãäåæçèéêëìíîïðñòóôõöøùúûüýÿ" %>% toutf
    utf <- system.file(package = "harmonizer") %>%
        file.path("enc/enc.txt") %>%
        readLines %>%
        enc2utf8
    ascii <- "SOZsozYYuAAAAAAACEEEEIIIIDNOOOOOOUUUUYsaaaaaaaceeeeiiiionoooooouuuuyy"
    if(enc.detect == "all")  # detect encoding of whole vector
        iconv(str
            , from = stri_enc_detect(str)[[1]]$Encoding[1]
            , to = "UTF-8"
            , sub = "") %>% 
            {chartr(utf, ascii, .)} %>% 
            iconv(to = "ASCII", sub = "")
    else if(enc.detect == "ind")  # detect encoding of individual elements
        lapply(str, function(el) iconv(el
                                     , from = stri_enc_detect(el)[[1]]$Encoding[1]
                                     , to = "UTF-8"
                                     , sub = "")) %>%
            unlist %>% 
            {chartr(utf, ascii, .)} %>% 
            iconv(to = "ASCII", sub = "")
    else
        enc2utf8(str) %>% 
            {chartr(utf, ascii, .)} %>% 
            iconv(to = "ASCII", sub = "")
}


## Test
## toascii(c("fa\xE7ile"
##         , "fa\xc3\xa7ile")
##         , enc.detect = "ind")



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
## make.subs.table <- function(harm.files) {
##     harm.files %>%
##         lapply(function(file)
##             read.csv(file
##                    , header = FALSE
##                    , col.names = c("del", "ins")
##                    , as.is = TRUE
##                    , colClasses = c("character", "character")
##                    , na.strings = NULL)) %>%
##         rbindlist
## }



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



## Subsitutes strings
#' @import magrittr stringi stringr
standardize <- function(org.names, subs.files
                      , add.spaces = "both") {
    for(file in subs.files) {
        org.names %<>% trims
        if(add.spaces == "right") org.names %<>%  paste0(" ")  # add space at the end
        if(add.spaces == "both") org.names %<>% {paste0(" ",. , " ")}  # add space at the end
        subs.table <- read.csv(file
                             , header = FALSE
                             , col.names = c("del", "ins")
                             , as.is = TRUE
                             , colClasses = c("character", "character")
                             , na.strings = NULL
                             , comment.char = "#")
        org.names %<>% stri_replace_all_fixed(subs.table$del
                                            , subs.table$ins
                                            , vectorize_all = FALSE)
    }
    return(org.names)
}





## Load Substitution Rules
## ================================================================================

## Path for files with substitutions
harm.dir <- system.file(package = "harmonizer") %>%
    file.path("nber-pdp-harmonization")
## harm.dir <- file.path("../inst/nber-pdp-harmonization")  # for testing

## Substitutions
harm.subs.files <- c("nber-pdp-harmonization-(punctuation2_not_all-2,5,6,10).csv"
                   , "nber-pdp-harmonization-(standard_name-8).csv"
                   , "nber-pdp-harmonization-(non_corporates_univ-8).csv"
                   , "nber-pdp-harmonization-(nameonly_main-9).csv"
                   , "nber-pdp-harmonization-(derwent_standardisation_BHH-9).csv")

harm.subs.files.derwent <- c("nber-pdp-harmonization-(derwent_standardisation_BHH-9).csv")

harm.subs.files.sansderwent <- c("nber-pdp-harmonization-(punctuation2_not_all-2,5,6,10).csv"
                   , "nber-pdp-harmonization-(standard_name-8).csv"
                   , "nber-pdp-harmonization-(nameonly_main-9).csv")

harm.subs.files.punctuation <- "nber-pdp-harmonization-(punctuation2_not_all-2,5,6,10).csv"

## Removals
harm.rem.files <- c("nber-pdp-harmonization-(stem_name-8).csv"
                  ## , "nber-pdp-harmonization-(non_corporates_univ-8).csv"
                  , "additional.removals.2018-01-01.csv"
                    )

## Codes and clues for organization type
harm.codes.files <- c("nber-pdp-harmonization-(codes_org_type-9).csv")


## Magerman et al. (2006) harmonization
## harm.magerman <- system.file(package = "harmonizer") %>%
##     file.path("magerman-harmonization", "magerman-harmonization.csv") %>%
##     read.csv(header = FALSE, stringsAsFactors = FALSE)


## for testing
## harm.magerman <- file.path("..", "inst", "magerman-harmonization"
##                          , "magerman-harmonization.csv") %>%
##     read.csv(header = FALSE
##            , stringsAsFactors = FALSE
##              , strip.white = TRUE
##            , quote = "\"")


## Wrapers for standardizes
## ================================================================================
#' @import magrittr
standardize.nber <- function(org.names) {
    file.path(harm.dir, c(harm.subs.files
                        , harm.rem.files)) %>% 
        {standardize(org.names, .)}
}


standardize.derwent <- function(org.names) {
    file.path(harm.dir, c(harm.subs.files.derwent)) %>% 
        {standardize(org.names, .)}
}


#' @import magrittr
standardize.nber.sansremovals <- function(org.names) {
    file.path(harm.dir, harm.subs.files) %>% 
        {standardize(org.names, .)}
}

#' @import magrittr
#' it assumes that there is no removals for the first word
standardize.nber.removals <- function(org.names) {
    file.path(harm.dir, c(harm.rem.files)) %>% 
        {standardize(org.names, .
                   , add.spaces = "right")}
}


#' @import magrittr
standardize.nber.sansderwent.sansremovals <- function(org.names) {
    file.path(harm.dir, harm.subs.files.sansderwent) %>% 
        {standardize(org.names, .)}
}

#' @import magrittr
standardize.nber.sansderwent <- function(org.names) {
    file.path(harm.dir, c(harm.subs.files.sansderwent
                        , harm.rem.files)) %>% 
        {standardize(org.names, .)}
}

#' @import magrittr
standardize.punctuation <- function(org.names) {
    file.path(harm.dir, harm.subs.files.punctuation) %>% 
        {standardize(org.names, .)}
}









## This function combines all previous functions
#' @title Harmonizes organizational names. 
#'
#' @description
#' Returns harmonized version of organizational names.
#' @param org.names Character vector of organizational names to harmonize
#' @param quite Logical value indicating whether or not print messages about procedures progress
#' @param progress.by Numeric value that is used to split the org.names vector for showing percentage of completion. Default is 0 meaning not to split the vector and thus does not show progress percentage. Designed to be used for long strings.
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
                          "enc2utf8"
                        , "tolower"
                        ## , "html2txt"
                        , "clear.in.brackets"
                        , list("toascii", "ind")
                        , "toupper" 
                        , "standardize.nber.sansremovals"
                        , "standardize.nber.removals"
                        , "trims"
                      )
                    , quite = FALSE
                    , progress.by = 0
                      ) {
    ## check if procedures are specified
    if(length(procedures) == 0) {message("No harmonizing procedures are specified."); return()}
    ## Apply harmonization
    if(is.character(org.names)) {
        if(!quite) message("Running harmonizer procedures:")
        for(procedure in procedures) {
            if(!quite) packageStartupMessage("* ", procedure[[1]], " ..."
                                           , paste0(rep("\t"
                                                      , 5 - ((nchar(procedure[[1]]) + 6) %/% 8)))
                                           , appendLF = FALSE)
            if(progress.by & !quite) {
                i <- 1
                l <- length(org.names)
                n <- l %/% progress.by + 1
                packageStartupMessage("  0%"
                                           , appendLF = FALSE)
                org.names %<>%
                    split(rep(1:n
                            , each = progress.by
                            , length.out = l)) %>%
                    lapply(function(org.names.by) {
                        packageStartupMessage("\b\b\b\b", " "
                                            , ifelse(round(100/n * i) < 10 , " ", "")
                                            , round(100/n * i), "%"
                                            , appendLF = FALSE)
                        i <<- i + 1
                        org.names.by %>% 
                            list %>%
                            c(procedure[-1])  %>%        # add arguents at the end
                            {do.call(procedure[[1]], .)}
                    }) %>% unlist
            }
            else org.names %<>% 
                     list %>%
                     c(procedure[-1])  %>%        # add arguents at the end
                     {do.call(procedure[[1]], .)}
            if(progress.by & !quite) packageStartupMessage("\b\b\b\b", "DONE")
            if(!progress.by & !quite) packageStartupMessage("DONE")
        }
        return(org.names)
    } else {
        return(org.names)
    }
}





## Testing progress bar...
## s <- 1:7003
## N <- 100
## s %>% split(rep(1:(length(.) %/% N + 1)
##             , each = N , length.out = length(.))) %>% class


## harmonize(as.character(1:100000), progress.by = 1000) %>% class


## Examples
## --------------------------------------------------------------------------------

## org.names.test <- c("žŸong-ÂÃÇÈÏa\n\u00b5 &oacute;\u00b5<p>,  INt LTD &AMP; Co;  "
##                   , "<br> the $ (&AMP; C&oacute;MP comPANY) Ïotta")
## data.frame(original = org.names.test
##          , harmonized = harmonize(org.names.test))


## comp.example <- c("žŸong-ÂÃÇÈÏa\n\u00b5 &oacute;\u00b5<p>,  INt LTD &AMP; Co;  "
##                 , "<br> the $ (&AMP; C&oacute;MP comPANY) Ïotta"
##                 , "Tempshield Cryo-Protection™"
##                 , "Ábcdêãçoàúü"
##                 , "Polgen Sp. z o.o. <U+0096> Sp. K."
##                 , "Polgen Sp. z o.o. – Sp. K."
##                 , "Jerome® <br>"
##                 , "Controlled Environments®  Magazine"
##                 , "a\n\u00b5\u00b5"
##                 , "fa\xE7ile"
##                 , "fa\xc3\xa7ile"
##                 , "MSlab CO. CO., LTD."
##                 , "MSlab, A \\SOCIETE ANONYME\\"
##                 , "S.A.S. University Co., {PE}, Ltd. (Europe)"
##                 , "Analytical Technologies Limited"
##                 , "Anasys Instruments Corporation"
##                 , "C4 Control de Contaminacion Ltda"
##                 , "Crescent Scientific Pvt Ltd."
##                 , "Daigger & Co., Inc."
##                 , "Dell Inc."
##                 , "Deltalab. S.L.U."
##                 , "DLAB Scientific Co.,Ltd."
##                 , "ebro Electronic GmbH und Co. KG"
##                 , "Ecom spol. s r.o., s.r.o., akc. spol."
##                 , "G.A.S. mbH"
##                 , "Glassco Laboratory Equipments PVT LTD"
##                 , "Lhasa Limited"
##                 , "rose plastic USA, LLLP")


## comp.example %>% harmonize(procedures = list(
##                                "enc2utf8"
##                                 , "tolower"
##                                  ## , "html2txt"
##                                , "clear.in.brackets"
##                                 , list("toascii", FALSE)
##                                , "toupper" 
##                                , "standardize.nber.sansremovals"
##                                , "standardize.nber.removals"
##                                , "trims"
##                           ))


## data.frame(origin = (harm.magerman[[1]] %>%
##                      {paste0("MSlab", .)})
##          , harm = (harm.magerman[[1]] %>%
##                    {paste0("MSlab", .)} %>%
##                    harmonize(procedures = list(
##                                  "toutf"
##                                , "tolower"
##                                  ## , "html2txt"
##                                , "clear.in.brackets"
##                                , "toascii"
##                                , "toupper"
##                                , "standardize.nber"
##                                , "standardize.nber.sansremovals"
##                                , "standardize.nber.removals"
##                                , "trims"
##                              )))
##          , magerman = (harm.magerman[[3]] %>% 
##                        {paste0("MSlab", .)})) %>%
##     write.csv("test.csv")
