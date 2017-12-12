## Load required packages
## --------------------------------------------------------------------------------
packages <- c("devtools"
            , "roxygen2")
packages.check <- lapply(packages, function(p) {
    if (!require(p, character.only = TRUE)) {
        install.packages(p
                       , repos = 'http://cloud.r-project.org'
                       , dependencies = TRUE)
        library(p, character.only = TRUE)}})




## Making a package
## --------------------------------------------------------------------------------
## Assume that it executes in the directory of the source
options(devtools.desc.author = paste0("'",
                                      person(
                                          "Stas", 
                                          "Vlasov", 
                                          email = "stanislav.a.vlasov@gmail.com", 
                                          role  = c("aut", "cre")
                                      ), "'"))
create("harmonizer"
     , description = list(Title  = "Harmonization of Organizational Names"
                        , Date = "2017-06-03"
                        , License = "MIT License"
                        , Imports = paste("pbapply"
                                        , "data.table"
                                        , "magrittr"
                                        , "XML"
                                        , "stringi"
                                        , "stringr" , sep = ", ")
                        , Description = "Harmonizes organizational names using steps described in Thoma et al. (2010) and Magerman, Looy, Bart, & Song (2006)."
                        , References = "Magerman, T., Looy, V., Bart, & Song, X. (2006). Data Production Methods for Harmonized Patent Statistics: Patentee Name Harmonization (SSRN Scholarly Paper No. ID 944470). Rochester, NY: Social Science Research Network. Retrieved from http://papers.ssrn.com/abstract=944470, Thoma, G., Torrisi, S., Gambardella, A., Guellec, D., Hall, B. H., & Harhoff, D. (2010). Harmonizing and combining large datasets - an application to firm-level patent and accounting data. National Bureau of Economic Research Working Paper Series, (15851). Retrieved from http://www.nber.org/papers/w15851 http://www.nber.org/papers/w15851.pdf"))



getwd()

setwd("/Users/stas/mega/research/harmonizer.r-package")

setwd("harmonizer")

roxygenise()

setwd("..")

install("~/mega/research/harmonizer.r-package/harmonizer")
library("harmonizer")



harm(list(c("Lala Ltd.", "bla-bla Ltd."),c("Lala Univ", "bla-bla Hosp")))


harmonize(c("Lala Ltd.", "bla-bla Ltd."))
