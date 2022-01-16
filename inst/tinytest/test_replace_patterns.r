## -------->>  [[file:../../harmonizer.src.org::*replace_patterns][replace_patterns:2]]
require(data.table)
## Test replace_patterns

expect_equal(
replace_patterns(data.table(x.lala = c("lala MSlab MSlab"
                                      , "this company called TriloBit.? maybe"
                                      , "MS007lab, Ltd. Ltd.")
                           , x.rows = c(TRUE, TRUE, FALSE)
                           , harm = c(1,25,"MSlab"))
                , patterns = c("MSlab", "TriloBit.?", "[0-3]+", "Ltd.")
                , patterns_type = c("regex", "fixed", "regex", "ends")
                , col = 1
                , rows = c(TRUE, TRUE, FALSE)
                , patterns_replacements_col = 1
                , patterns_mode = "first")
, data.table(x.lala = c("lala  MSlab", "this company called  maybe", 
                        "MS007lab, Ltd. Ltd."), x.rows = c(TRUE, TRUE, FALSE), harm = c("1", 
                                                                                        "25", "MSlab")))



## multy mode test
expect_equal(
  replace_patterns(data.table(x.lala = c("lala MSlab MSlab"
               , "this company called TriloBit.? maybe TriloBit.?"
               , "MS007lab, Ltd. Ltd.")
         , x.rows = c(TRUE, FALSE, TRUE)
         , harm = c(1,25,"MSlab"))
          , patterns = c("MSlab", "TriloBit.?", "[0-3]+", "Ltd.")
                  , patterns_type = c("regex", "fixed", "regex", "fixed")
                  , col = 1
                  , x.rows = c(TRUE, FALSE, TRUE)
                  , patterns_replacements_col = 1
         , patterns_mode = c("last", "first", "all", "all"))[] 
, data.table(x.lala = c("lala MSlab ", "this company called  maybe TriloBit.?", 
"MS7lab,  "), x.rows = c(TRUE, FALSE, TRUE), harm = c("1", "25", 
"MSlab")))




## patterns as table test
expect_equal(
  replace_patterns(data.frame(x.lala = c("lala MSlab MSlab"
               , "this company called TriloBit.? maybe TriloBit.?"
               , "MS007lab, Ltd. Ltd.")
         , x.rows = c(TRUE, FALSE, TRUE)
         , harm = c(1,25,"MSlab"))
           , patterns =
                      data.frame(pats = c("MSlab", "TriloBit.?", "[0-3]+", "Ltd.")
                               , type = c("regex", "fixed", "regex", "ends")
                               , mode = c("all", "first", "all", "all")
                               , replacements = c("MSMS", "TBTB", "NRNR", "COMP"))
                  , col = 1
                  , rows = c(TRUE, FALSE, TRUE)
                  , patterns_mode_col = 3
                  , patterns_type_col = "type" 
         , patterns_replacements_col = "replacements")
, data.table(x.lala = c("lala MSMS MSMS", "this company called TriloBit.? maybe TriloBit.?", 
"MSNRNR7lab, Ltd. COMP"), x.rows = c(TRUE, FALSE, TRUE), harm = c("1", 
                                                                  "25", "MSlab"))
)


## test for all x.rows FALSE
expect_equal(
    replace_patterns(data.frame(x.lala = c("lala MSlab MSlab"
               , "this company called TriloBit.? maybe TriloBit.?"
               , "MS007lab, Ltd. Ltd.")
         , x.rows = c(TRUE, FALSE, TRUE)
         , harm = c(1,25,"MSlab"))
           , patterns =
                      data.frame(pats = c("MSlab", "TriloBit.?", "[0-3]+", "Ltd.")
                               , type = c("regex", "fixed", "regex", "ends")
                               , mode = c("all", "first", "all", "all")
                               , replacements = c("MSMS", "TBTB", "NRNR", "COMP"))
                  , patterns_type_col = "type"
                  , col = 1
                  , col.update = TRUE
                  , rows = c(FALSE, FALSE, FALSE)
                  , patterns_replacements_col = "replacements"
         , patterns_mode_col = 3)
, data.table(x.lala = c("lala MSlab MSlab", "this company called TriloBit.? maybe TriloBit.?", 
"MS007lab, Ltd. Ltd."), x.rows = c(TRUE, FALSE, TRUE), harm = c("1", 
"25", "MSlab")))
## --------<<  replace_patterns:2 ends here


