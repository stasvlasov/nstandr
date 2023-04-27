## -------->>  [[file:../../nstandr.src.org::*magerman.replace.sgml.characters][magerman.replace.sgml.characters:2]]
test.df <-
data.frame( a = 
c("&AMP;&OACUTE;&SECT; 02937lkjfas;ldjf  &UACUTE;&#8902;&BULL; sdlfkjhhhh ;laskdjf&EXCL;"
,   "&AMP;&OACUTE;&SECT; 02937lkjfas;ldjf  &UACUTE;&#8902;&BULL; sdlfkjhhhh ;laskdjf&EXCL;")
, b = c("swe"
, "w3r"))

expect_equal(magerman_replace_sgml_characters(test.df)
, structure(list(a = c("&Ó§ 02937lkjfas;ldjf  Ú . sdlfkjhhhh ;laskdjf!", 
"&Ó§ 02937lkjfas;ldjf  Ú . sdlfkjhhhh ;laskdjf!"), b = c("swe", 
"w3r")), row.names = c(NA, -2L), class = c("data.table", "data.frame")))
## --------<<  magerman.replace.sgml.characters:2 ends here


