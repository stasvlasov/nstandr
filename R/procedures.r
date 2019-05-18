## this should accept both vector and table and return eather vector or a table
magerman.replace.sgml.characters <- function(x
                                           , harmonize.col = 1
                                           , bind.cols = c("none"
                                                         , "harmonize.col"
                                                         , "all.but.harmonize.col"
                                                         , "all")[1]) {
  it.is.vector <- is.vector(x)
  x %>%
    {if(it.is.vector) . else .[[harmonize.col]]} %>%
    stri_replace_all_fixed(magerman.patterns.sgml.characters[[1]]
                         , magerman.patterns.sgml.characters[[2]]
                         , vectorize_all = FALSE) %>%
    {if(bind.cols == "none" | (it.is.vector & bind.cols == "all.but.harmonize.col")) .
     else if(it.is.vector & (bind.cols == "all" | bind.cols == "harmonize.col"))
       cbind(harmonized = ., data.table(original = x))
     else if(bind.cols == "all")
       cbind(harmonized = ., as.data.table(x))
     else if(bind.cols == "all.but.harmonize.col")
       cbind(harmonized = ., as.data.table(x) %>% select(-harmonize.col))
     else if(bind.cols == "harmonize.col")
       cbind(harmonized = ., as.data.table(x) %>% select(harmonize.col))
     }
}

  ## test
data.frame(
  c("&AMP;&OACUTE;&SECT; 02937lkjfas;ldjf  &UACUTE;&#8902;&BULL; sdlfkjhhhh ;laskdjf&EXCL;"
,   "&AMP;&OACUTE;&SECT; 02937lkjfas;ldjf  &UACUTE;&#8902;&BULL; sdlfkjhhhh ;laskdjf&EXCL;")
, c("swe"
  , "w3r")) %>%
  magerman.replace.sgml.characters(bind.cols = "all.but.harmonize.col")



"&AMP;&OACUTE;&SECT; 02937lkjfas;ldjf  &UACUTE;&#8902;&BULL; sdlfkjhhhh ;laskdjf&EXCL;" %>% magerman.replace.sgml.characters(bind.cols = "all.but.harmonize.col")



c("swe", "w3r", 3) %>% is.vector


cbind(c("swe", "w3r", 3)
    , original = data.frame(c("swe", "w3r", 3), c("swe", "w3r", 3)))



cbind(c("swe", "w3r", 3)
     ,as.data.table(c("swe", "w3r", 3))) %>% class



%>% is.vector

is.null(c("swe", "w3r", 3) %>% ncol)

if (NULL) 2

magerman.replace.proprietary.characters <- function(x) {
  stri_replace_all_fixed(x
                        , magerman.patterns.proprietary.characters[[1]]
                        , magerman.patterns.proprietary.characters[[2]]
                        , vectorize_all = FALSE)
}

## test
## "&AMP;&OACUTE;&SECT; 02937lkjfas;ldjf  &UACUTE;&#8902;&BULL; sdlfkjhhhh ;laskdjf&EXCL;" %>%
##   magerman.replace.sgml.characters
