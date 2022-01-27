## -------->>  [[file:../harmonizer.src.org::*cockburn.combabbrev][cockburn.combabbrev:1]]
##' Collapses single character sequences
##'
##' @param x Object (table or vector)
##' @param wrap_in_spaces Whether to wrap strings in spaces before processing because the algorithm assumes assumes that each string begins and ends with space. Default is TRUE.
##' @inheritDotParams harmonize.x
##' @return Harmonized table
##' 
##' @md 
##' @import magrittr
##' @export 
cockburn.combabbrev <- function(x
                              , wrap_in_spaces = TRUE
                              , ...) {
  x.vector <-
    harmonize.x(x, ...) %>%
    ## wrap in spaces
    {if(wrap_in_spaces) paste0(" ", ., " ") else .}
  ## collapse
  sapply(x.vector, function(org.name) {
    reg  <- gregexpr("(?=\\s\\w(\\s+)\\w\\s)", org.name, perl = TRUE)
    ## check if there are matches
    if(reg[[1]][1] != -1) {
      char <- strsplit(org.name, "", fixed = TRUE) %>% unlist
      pos <- mapply(function(from, length.out) seq(from, length.out = length.out)
                  , from = attr(reg[[1]],"capture.start")
                  , length.out = attr(reg[[1]],"capture.length")
                  , SIMPLIFY = FALSE) %>% unlist
      char %>% inset(pos, "") %>% paste(collapse = "")
    } else org.name
  }) %>% harmonize.x(x, ., ...)
}

## ## test
## c(" A B Comp"
## , " A  B Comp a"
## , " I B M "
## , "I B M bla-bla n bla C O") %>%
##   cockburn.combabbrev

## data.table(name = c("MÄKARÖNI ETÖ FKÜSNÖ Ltd"
##                   , "MSLab CÖ. <a href=lsdldf> <br> <\\a>"
##                   , " M S Lab Co."
##                   , "MSLaeb Comp."
##                   , "MSLab Comp. Ltd."
##                   , "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝŸ") %>% rep(20)
##          , foo = "I love coffee" ) %>%
##   cockburn.combabbrev


## c("MÄKARÖNI ETÖ FKÜSNÖ Ltd"
## , "MSLab CÖ. <a href=lsdldf> <br> <\\a>"
## , "MSLab Co."
## , "MSLaeb Comp."
## , "MSLab Comp. Ltd."
## , "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝŸ") %>%
##   cockburn.combabbrev
## --------<<  cockburn.combabbrev:1 ends here



## -------->>  [[file:../harmonizer.src.org::*Derwent][Derwent:1]]
##' Performs Derwent standardization of organizational names
##'
##' It is a version from Cockburn, I. M., A. Agrawal, J. Bessen, J. H. S. Graham, B. H. Hall, and M. MacGarvie (2009), The NBER Patent Citations Datafile Update. It differs from original dervert standartization
##' @param x object
##' @inheritDotParams replace_patterns
##' @return Harmonized table
##' 
##' @md 
##' @export 
cockburn.replace.derwent <- function(x
                           , ...) {
  replace_patterns(x
                  , patterns = cockburn.patterns.derwent
                  , patterns.mode = "first"
                  , ...)
}
## --------<<  Derwent:1 ends here



## -------->>  [[file:../harmonizer.src.org::*Compustat][Compustat:1]]
##' COMPUSTAT specific standardization for organizational names
##'
##' @param x object
##' @inheritDotParams replace_patterns
##' @return Harmonized table
##' 
##' @md 
##' @export 
cockburn.replace.compustat <- function(x
                             , ...) {
  replace_patterns(x
                  , patterns = cockburn.patterns.compustat
                  , ...)
}



##' COMPUSTAT specific standardization for organizational names. Full name replacements.
##'
##' @param x object
##' @inheritDotParams replace_patterns
##' @return Harmonized table
##' 
##' @md 
##' @export 
cockburn.replace.compustat.names <- function(x
                                   , ...) {
  replace_patterns(x
                  , patterns = cockburn.patterns.compustat.names
                  , patterns.type = "trim.exact"
                  , ...)
}

## Test
## c("WESTINGHOUSE ELEC  "
## , "GRACE (W R) & CO"
## , "GRACE (W R) & CO Ltd.") %>% 
##   cockburn.compustat.names
## --------<<  Compustat:1 ends here



## -------->>  [[file:../harmonizer.src.org::*Identify Entity Type][Identify Entity Type:1]]
##' Identifies Entity Type
##'
##' @param x vector or table
##' @param procedures.message For debuging. If set will message which procedures were done.
##' @inheritDotParams harmonize.detect
##' @return Harmonized table
##' 
##' @md 
##' @export 
cockburn.detect.type <- function(x
                               , procedures.message = FALSE
                               , ...) {
  procedure.message <- function(x, name) {
    if(procedures.message) message(name)
    return(x)
  }
  x %>% 
    cockburn.detect.corp(...) %T>% 
    procedure.message("cockburn.detect.corp") %>%
    cockburn.detect.indiv(...) %T>% 
    procedure.message("cockburn.detect.indiv") %>%
    cockburn.detect.govt(...) %T>% 
    procedure.message("cockburn.detect.govt") %>%
    cockburn.detect.univ(...) %T>% 
    procedure.message("cockburn.detect.univ") %>%
    cockburn.detect.inst(...) %T>% 
    procedure.message("cockburn.detect.inst") %T>%
    cockburn.detect.inst.conds(...) %T>% 
    procedure.message("cockburn.detect.inst.conds") %>%
    cockburn.detect.inst.german(...) %T>% 
    procedure.message("cockburn.detect.inst.german") %>%
    cockburn.detect.hosp(...) %T>% 
    procedure.message("cockburn.detect.hosp") %>%
    return()
}

##' Cleanup Entity Type
##'
##' @param x vector or table
##' @inheritDotParams replace_patterns
##' @return Harmonized table
##' 
##' @md 
##' @export 
cockburn.replace.type <- function(x, ...) {
  x %>% 
    cockburn.replace.govt(...) %>% 
    cockburn.replace.univ(...) %>% 
    return()
}
## --------<<  Identify Entity Type:1 ends here



## -------->>  [[file:../harmonizer.src.org::*Firms (Corporates)][Firms (Corporates):1]]
##' Detect Corporates (code - "firm")
##'
##' From non_corporates.do file. Source - https://sites.google.com/site/patentdataproject/Home/posts/namestandardizationroutinesuploaded
##' 
##' @param x vector or table
##' @param codes.name 
##' @param x.codes.merge 
##' @param ... 
##' @inheritDotParams harmonize.detect
##' @return Harmonized table
##' 
##' @md 
##' @export 
cockburn.detect.corp <- function(x
                               , codes.name = "entity.type"
                               , x.codes.merge = TRUE 
                               , ...) {
  harmonize.detect(x
                 , codes.name = codes.name
                 , x.codes.merge = x.codes.merge
                 , patterns = cockburn.patterns.corp
                 , codes = "firm"
                 , return.only.first.detected.code = TRUE
                 , ...)
}

## test
## c(" DR VLASOV & BROTHER "
## ,  "MSlab & C"
## , " S.VLASOV PHD "
## , "LEGALY REPRESENTED BY STAS") %>%
## cockburn.detect.corp
## --------<<  Firms (Corporates):1 ends here



## -------->>  [[file:../harmonizer.src.org::*Individuals][Individuals:1]]
##' Detect Individuals (Non-Corporates group)
##'
##' From non_corporates.do file. Source - https://sites.google.com/site/patentdataproject/Home/posts/namestandardizationroutinesuploaded
##' 
##' @param x vector or table
##' @inheritDotParams harmonize.detect
##' @return Harmonized table
##' 
##' @md 
##' @export 
cockburn.detect.indiv <- function(x
                               , codes.name = "entity.type"
                               , x.codes.merge = TRUE
                               , ...) {
  harmonize.detect(x
                 , codes.name = codes.name
                 , x.codes.merge = x.codes.merge
                 , patterns = cockburn.patterns.indiv
                 , codes = "indiv"
                 , return.only.first.detected.code = TRUE
                 , ...)
}

## test
## c(" DR VLASOV "
## , " S.VLASOV PHD "
## , "LEGALY REPRESENTED BY STAS") %>%
## cockburn.detect.indiv

  ## c(" DR VLASOV "
  ## , " S.VLASOV PHD "
  ## , " STANICA LEGALY REPRESENTED BY STAS"
  ## , " DR VLASOV & BROTHER "
  ## , "MSlab & C"
  ## , "LEGALY REPRESENTED BY STAS"
  ## , " REPUBLIC LEGALY REPRESENTED BY STAS"
  ## , " TILBURG UNIVERSTIY "
  ## , " VU UNIVERSTITAET "
  ## , " FUNDATION LEGALY REPRESENTED BY STAS") %>%
  ##   cockburn.detect.indiv %>%
  ##   cockburn.detect.govt %>%
  ##   cockburn.detect.indiv
## --------<<  Individuals:1 ends here



## -------->>  [[file:../harmonizer.src.org::*Government][Government:1]]
##' Detect Goverment Organizations (Non-Corporates group)
##'
##' From non_corporates.do file. Source - https://sites.google.com/site/patentdataproject/Home/posts/namestandardizationroutinesuploaded
##' 
##' @param x vector or table
##' @inheritDotParams harmonize.detect
##' @return Harmonized table
##' 
##' @md 
##' @export 
cockburn.detect.govt <- function(x
                               , codes.name = "entity.type"
                               , x.codes.merge = TRUE 
                               , ...) {
  harmonize.detect(x
                 , codes.name = codes.name
                 , x.codes.merge = x.codes.merge
                 , patterns = cockburn.patterns.govt
                 , codes = "govt"
                 , return.only.first.detected.code = TRUE
                 , ...)
}

## test
## c(" DR VLASOV "
## , " S.VLASOV PHD "
## , " REPUBLIC LEGALY REPRESENTED BY STAS") %>%
## cockburn.detect.govt


##' Cleanup Goverment Organizations (Non-Corporates group)
##'
##' From non_corporates.do file. Source - https://sites.google.com/site/patentdataproject/Home/posts/namestandardizationroutinesuploaded
##' 
##' @param x vector or table
##' @inheritDotParams harmonize.detect
##' @return Harmonized table
##' 
##' @md 
##' @export 
cockburn.replace.govt <- function(x, ...) {
    replace_patterns(x
                    , patterns = cockburn.patterns.govt.cleanup
                    , ...)
}


## test
## " VERY IMPORTANT SEC OF THE DEPT OF  " %>% cockburn.replace.govt
## --------<<  Government:1 ends here



## -------->>  [[file:../harmonizer.src.org::*Universities][Universities:1]]
##' Detect Universities (Non-Corporates group)
##'
##' From non_corporates.do file. Source - https://sites.google.com/site/patentdataproject/Home/posts/namestandardizationroutinesuploaded
##' 
##' @param x vector or table
##' @inheritDotParams harmonize.detect
##' @return Harmonized table
##' 
##' @md 
##' @export 
cockburn.detect.univ <- function(x
                               , codes.name = "entity.type"
                               , x.codes.merge = TRUE 
                               , ...) {
  harmonize.detect(x
                 , codes.name = codes.name
                 , x.codes.merge = x.codes.merge
                 , patterns = cockburn.patterns.univ
                 , codes = "univ"
                 , return.only.first.detected.code = TRUE
                 , ...)
}

## test
## c(" TILBURG UNIVERSTIY "
## , " VU UNIVERSTITAET "
## , "LEGALY REPRESENTED BY STAS") %>%
## cockburn.detect.univ


##' Cleanup Universities (Non-Corporates group)
##'
##' From non_corporates.do file. Source - https://sites.google.com/site/patentdataproject/Home/posts/namestandardizationroutinesuploaded
##' 
##' @param x vector or table
##' @inheritDotParams harmonize.detect
##' @return Harmonized table
##' 
##' @md 
##' @export 
cockburn.replace.univ <- function(x, ...) {
    replace_patterns(x
                    , patterns = cockburn.patterns.univ.cleanup
                    , ...)
}

## test
## c(" SUPERVISORS OF THE TILBURG UNIVERSTIY "
## , " VU UNIVERSTITAET "
## , "LEGALY REPRESENTED BY STAS") %>%
## cockburn.replace.univ
## --------<<  Universities:1 ends here



## -------->>  [[file:../harmonizer.src.org::*Non-profit institutes][Non-profit institutes:1]]
##' Detect Non-profit Institutes (Non-Corporates group)
##'
##' From non_corporates.do file. Source - https://sites.google.com/site/patentdataproject/Home/posts/namestandardizationroutinesuploaded
##' 
##' @param x vector or table
##' @inheritDotParams harmonize.detect
##' @return Harmonized table
##' 
##' @md 
##' @export 
cockburn.detect.inst <- function(x
                               , codes.name = "entity.type"
                               , x.codes.merge = TRUE 
                               , ...) {
  harmonize.detect(x
                 , codes.name = codes.name
                 , x.codes.merge = x.codes.merge
                 , patterns = cockburn.patterns.inst
                 , codes = "inst"
                 , return.only.first.detected.code = TRUE
                 , ...)
}

## test
## c(" DR VLASOV "
## , " S.VLASOV PHD "
## , " FUNDATION LEGALY REPRESENTED BY STAS") %>%
## cockburn.detect.inst
## --------<<  Non-profit institutes:1 ends here



## -------->>  [[file:../harmonizer.src.org::*Complex conditions][Complex conditions:1]]
##' Detects Non-profit institutes with special conditions
##'
##' @param x table. Expected that x has a column with codes for universities
##' @param codes.col column with codes for universities ("univ"). Default is last column of x
##' @inheritDotParams harmonize.detect
##' @return Harmonized table
##' 
##' @md
##' @export
cockburn.detect.inst.conds.1 <- function(x
                                       , codes.name = "entity.type"
                                       , x.codes.merge = TRUE
                                       , ...) {
    ## STATA equvalent
    ## replace asstype = "inst" if strpos(standard_name," COUNCIL OF ")>0 & strpos(standard_name," RES ")>0
    harmonize.detect(x
                   , patterns = " COUNCIL OF .* RES | RES .* COUNCIL OF "
                   , patterns.type = "regex"
                   , codes = "inst"
                   , codes.name = codes.name
                   , x.codes.merge = x.codes.merge
                   , return.only.first.detected.code = TRUE
                   , ...)
        }


##' Detects Non-profit institutes with special conditions
##'
##' @param x table. Expected that x has a column with codes for universities
##' @param codes.col column with codes for universities ("univ"). Default is last column of x
##' @inheritDotParams harmonize.detect
##' @return Harmonized table
##' 
##' @md
##' @export
cockburn.detect.inst.conds.2 <- function(x
                                       , codes.name = "entity.type"
                                       , x.codes.merge = TRUE
                                       , ...) {
    ## STATA equvalent
    ## replace asstype = "inst" if strpos(standard_name," FOUND ")~=0 & asstype~="univ"
    ## assume a bug: " FOUND ")~=0 -> " FOUND ")>0
    ## replace asstype = "inst" if strpos(standard_name," INST ")>0 & asstype~="univ"
    codes <- harmonize.x(x, x.col = codes.name, ...)
    conds <- codes %>%
        lapply(equals, "univ") %>% 
        sapply(any, na.rm = TRUE) %>%
        not
    x.vec <- harmonize.x(x, ...)
    coded <- harmonize.detect(x = data.table(x.vec, codes)
                            , patterns = c(" FOUND "
                                         , " INST ")
                            , x.rows = conds
                            , codes = "inst"
                            , x.codes.merge = x.codes.merge
                            , return.only.first.detected.code = TRUE
                            , return.only.codes = TRUE)
    harmonize.x.dots(x, coded, x.col = codes.name
                   , x.col.update = TRUE)
}




##' Detects Non-profit institutes with special conditions
##'
##' @param x table. Expected that x has a column with codes for universities
##' @param codes.col column with codes for universities ("univ"). Default is last column of x
##' @inheritDotParams harmonize.detect
##' @return Harmonized table
##' 
##' @md
##' @export
cockburn.detect.inst.conds <- function(x
                                     , x.codes.merge = TRUE
                                     , codes.name = "entity.type"
                                     , ...) {
  x %>% 
    cockburn.detect.inst.conds.1(x.codes.merge = x.codes.merge
                               , codes.name = codes.name) %>%
    cockburn.detect.inst.conds.2(codes.name = codes.name
                               , x.codes.merge = x.codes.merge) %>%
    return()
}
## --------<<  Complex conditions:1 ends here



## -------->>  [[file:../harmonizer.src.org::*German Non-profit institutes][German Non-profit institutes:1]]
##' Detects German Non-profit institutes
##'
##' "EINGETRAGENER VEREIN. NON PROFIT SOCIETY/ASSOCIATION."
##' 
##' @param x table
##' @inheritDotParams harmonize.detect
##' @return Harmonized table
##' 
##' @md 
##' @export 
cockburn.detect.inst.german <- function(x
                               , codes.name = "entity.type"
                               , x.codes.merge = TRUE 
                               , ...) {
  conds <- harmonize.detect(x, patterns = c(" UNIV "
                                          , " GMBH "
                                          , " KGAA "
                                          , " KG "
                                          , " AG "
                                          , " EG "
                                          , " OHG ")
                          , codes = TRUE
                          , return.only.first.detected.code = TRUE
                          , return.only.codes = TRUE) %>%
      unlist %>% sapply(isTRUE) %>% not
  harmonize.detect(x, patterns = c(" STIFTUNG "
                                 , " EINGETRAGENER VEREIN ")
                 , codes.name = codes.name
                 , x.codes.merge = x.codes.merge
                 , x.rows = conds
                 , codes = "inst"
                 , return.only.first.detected.code = TRUE
                 , ...)
}
## --------<<  German Non-profit institutes:1 ends here



## -------->>  [[file:../harmonizer.src.org::*Hospitals][Hospitals:1]]
##' Detect Hospitals (Non-Corporates group)
##'
##' From non_corporates.do file. Source - https://sites.google.com/site/patentdataproject/Home/posts/namestandardizationroutinesuploaded
##' 
##' @param x vector or table
##' @inheritDotParams harmonize.detect
##' @return Harmonized table
##' 
##' @md 
##' @export 
cockburn.detect.hosp <- function(x
                               , codes.name = "entity.type"
                               , x.codes.merge = TRUE 
                               , ...) {
  harmonize.detect(x
                 , codes.name = codes.name
                 , x.codes.merge = x.codes.merge
                 , patterns = cockburn.patterns.hosp
                 , codes = "hosp"
                 , return.only.first.detected.code = TRUE
                 , ...)
}




## test
## c(" DR VLASOV "
## , " S.VLASOV PHD "
## , " STANICA LEGALY REPRESENTED BY STAS") %>%
##     cockburn.detect.hosp
## --------<<  Hospitals:1 ends here



## -------->>  [[file:../harmonizer.src.org::*Punctuation][Punctuation:1]]
##' Removes punctuation and standardise some symbols. 
##'
##' @param x object
##' @inheritDotParams replace_patterns
##' @return Harmonized table
##' 
##' @md
##' @import magrittr
##' @export 
cockburn.replace.punctuation <- function(x
                                         , ...) {
  x %>%
    harmonize.replace(patterns = cockburn.patterns.punctuation.and, ...) %>%
    harmonize.replace(patterns = cockburn.patterns.punctuation.the
                    , patterns.type.col = 3, ...) %>%
    ## I swapted patstat with amadeus otherwise Ã²Ã¢ÃªÃ®Ã© will not become oaeie
    harmonize.replace(patterns = cockburn.patterns.punctuation.patstat, ...) %>% 
    harmonize.replace(patterns = cockburn.patterns.punctuation.amadeus, ...) %>%
    harmonize.replace(patterns = cockburn.patterns.punctuation.char, ...)
}

## Test
## c("WESTINGHOUSE, |.?^&*@ ELEC  "
## , "GRACE (W EN R) & CO - Ã²Ã¢ÃªÃ®Ã©"
## , "GRACE (W/R) & CO Ltd.") %>% 
##   cockburn.replace.punctuation
## --------<<  Punctuation:1 ends here



## -------->>  [[file:../harmonizer.src.org::*Standard Name][Standard Name:1]]
##' Create standard name
##'
##' @param x object
##' @inheritDotParams replace_patterns
##' @return Harmonized table
##' 
##' @md
##' @import magrittr
##' @export 
cockburn.replace.standard.names <- function(x
                                            , ...) {
  x %>%
    cockburn.replace.derwent(...) %>% 
    replace_patterns(patterns = cockburn.patterns.standard.names.additional, ...) %>% 
    replace_patterns(patterns = cockburn.patterns.standard.names.country.specific, ...)
}
## --------<<  Standard Name:1 ends here



## -------->>  [[file:../harmonizer.src.org::*Stem Name][Stem Name:1]]
##' Creates so called stem name (a name with all legal entity identifiers removed)
##'
##' @param x object
##' @inheritDotParams replace_patterns
##' @return Harmonized table
##' 
##' @md
##' @import magrittr
##' @export 
cockburn.remove.standard.names <- function(x
                                            , ...) {
  replace_patterns(x
                  , patterns = cockburn.patterns.stem.name
                  , replacements = " "
                  , ...) 
}

## Test
## c("WESTINGHOUSE, |.?^&*@ ELEC  "
## , "GRACE (W EN R) & CO - Ã²Ã¢ÃªÃ®Ã©  PUBLIC LIMITED "
## , "GRACE (W/R) & CO LTD ") %>% 
##  cockburn.remove.standard.names
## --------<<  Stem Name:1 ends here



## -------->>  [[file:../harmonizer.src.org::*USPTO special][USPTO special:1]]
##' Removes special USPTO codes.
##'
##' @param x object
##' @inheritDotParams replace_patterns
##' @return Harmonized table
##' 
##' @md
##' @import magrittr
##' @export 
cockburn.remove.uspto <- function(x
                                     , ...) {
  replace_patterns(x, patterns = cockburn.patterns.uspto, ...) 
}



##' Special USPTO codes. Codes as "indiv"
##'
##' @param x object
##' @inheritDotParams harmonize.detect
##' @return Harmonized table
##' 
##' @md
##' @import magrittr
##' @export 
cockburn.detect.uspto <- function(x
                                     , ...) {
  harmonize.detect(x
                 , patterns = ";"
                 , codes = "indiv"
                 , codes.name = "entity.type"
                 , return.only.first.detected.code = TRUE
                 , ...) 
}

## ## Test
## c("WESTINGHOUSE, |.?^&*@ ELEC-CONN.  "
## , "GRACE-CONN. (W EN R) & CO - Ã²Ã¢ÃªÃ®Ã©  PUBLIC LIMITED "
## , "Bechara;John") %>% 
##  cockburn.remove.uspto.code

## c("WESTINGHOUSE, |.?^&*@ ELEC-CONN.  "
## , "GRACE-CONN. (W EN R) & CO - Ã²Ã¢ÃªÃ®Ã©  PUBLIC LIMITED "
## , "Bechara;John") %>% 
##   cockburn.detect.uspto.code
## --------<<  USPTO special:1 ends here



## -------->>  [[file:../harmonizer.src.org::*main_coname2.do][main_coname2.do:1]]
##' Harmonizes strings using exact procedures described in Cockburn, et al. (2009)
##' @param x table or vector
##' @param cockburn.procedures list of procedures to pass to `harmonize` function. Default is `cockburn.procedures.list`
##' @param detect.legal.form Whether to detect legal forms. Default is FALSE
##' @param return.x.before.common.words.removal Whether to save harmonized column before `common.words.removal` procedure. Default is FALSE
##' @param return.x.cols.all Whether to return initial column in x. Default is FALSE
##' @inheritDotParams harmonize
##' @return Harmonized table
##'
##' @references Cockburn, et al. (2009)
##' 
##' @md 
##' @import magrittr
##' @export 
harmonize.cockburn <- function(x
                             , cockburn.procedures = cockburn.procedures_table
                             , detect.legal.form = FALSE
                             , return.x.before.common.words.removal = FALSE
                             , return.x.cols.all = FALSE
                             , ... ) {
    if(is.data.frame(cockburn.procedures)) {
        cockburn.procedures %<>% harmonize_make_procedures_list
    }
  ## do some tweaks on cockburn.procedures
  if(!detect.legal.form) {
    cockburn.procedures %<>%
      extract(sapply(., extract2, 1) %>%
              is_in(c("cockburn.detect.type"
                    , "cockburn.detect.uspto")) %>%
              not)
  }
  if(return.x.before.common.words.removal) {
    cockburn.procedures %<>% 
      inset2(sapply(., extract2, 1) %>%
             equals("cockburn.remove.standard.names") %>%
             which
           , list("cockburn.remove.standard.names"
                , return.x.cols.all = TRUE))
  }
  if(return.x.cols.all) {
    cockburn.procedures %<>% 
      inset2(1, c(as.list(extract2(.,1))
                , return.x.cols.all = TRUE))
  }
  harmonize(x, cockburn.procedures, ...)
}
## --------<<  main_coname2.do:1 ends here


