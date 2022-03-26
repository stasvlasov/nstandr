## -------->>  [[file:../../nstandr.src.org::*Characters][Characters:2]]
expect_equal(
    magerman_detect_characters("Chip &AMP; Dayle (lala) [0x2345] {abs} ops html <br>")
  , structure(list(x = "Chip &AMP; Dayle (lala) [0x2345] {abs} ops html <br>", 
                   characters_cleaning_candidates = list(c("propriety coded characters {xxx}", 
                                                           "propriety coded characters [0xxx]", "propriety coded characters (xxx)", 
                                                           "sgml coded characters", "html coded characters"))), row.names = c(NA, 
                                                                                                                              -1L), class = c("data.table", "data.frame")))
## --------<<  Characters:2 ends here


