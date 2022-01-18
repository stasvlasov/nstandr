## -------->>  [[file:../../harmonizer.src.org::*magerman.replace.accented.characters][magerman.replace.accented.characters:2]]
expect_equal("ŠŒŽšœžŸ¥µÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝßàáâãäåæçèéêëìíîïðñòóôõöøùúûüýÿ" |>
             magerman_replace_accented_characters()
           , "ŠŒŽšœžY¥µAAAAAAAECEEEEIIIIÐNOOOOOØUUUUYßàáâãäåæçèéêëìíîïðñòóôõöøùúûüýÿ")

## somewhat works:
## [1] "ŠŒŽšœžY¥µAAAAAAAECEEEEIIIIÐNOOOOOØUUUUYßàáâãäåæçèéêëìíîïðñòóôõöøùúûüýÿ"
## --------<<  magerman.replace.accented.characters:2 ends here


