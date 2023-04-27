## -------->>  [[file:../../nstandr.src.org::*magerman.replace.proprietary.characters][magerman.replace.proprietary.characters:2]]
expect_equal("&AMP;&OACUTE;&SECT; {UMLAUT OVER (E)} sdlfkjhhhh ;laskdjf&EXCL;" |>
             magerman_replace_proprietary_characters()
           , "&AMP;&OACUTE;&SECT; Ë sdlfkjhhhh ;laskdjf&EXCL;")
## --------<<  magerman.replace.proprietary.characters:2 ends here


