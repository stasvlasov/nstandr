[![R-CMD-check](https://github.com/stasvlasov/harmonizer/workflows/R-CMD-check/badge.svg)](https://github.com/stasvlasov/harmonizer/actions)
[![codecov](https://codecov.io/gh/stasvlasov/harmonizer/branch/master/graph/badge.svg?token=OQVJ7NRXO5)](https://codecov.io/gh/stasvlasov/harmonizer)
![GitHub code size in bytes](https://img.shields.io/github/languages/code-size/stasvlasov/harmonizer)

The R package `harmonizer` makes standardized organizational names using procedures described by Thoma et al. (2010), Magerman et al. (2006), Cockburn et al. (2009).

This is work in progress. Please, file an issue or a suggestion if you have any.


# Installation

    devtools::install_github("stasvlasov/harmonizer")


# Usage

The package provides its main function `harmonize` that expect character vector of organization names as input and returns its harmonized version.

    c("žŸong-ÃÇÈÏ\n\u00b5&oacute;\u00b5<p>, LTD Co;  "
    , "<br> the $(Ldt &AMP; C&oacute;MP) Ïotta INt"
    , "Masha  &AMP;Lena Ltd. (Spb)"
    , "bla-bla-bla Ltd.") |>
        harmonize(output_placement = "append_to_x")
    
    #                                              x           std_x
    #  1        žŸong-ÃÇÈÏ\nµ&oacute;µ<p>, LTD Co;    ZYONG ACEI UOU
    #  2 <br> the $(Ldt &AMP; C&oacute;MP) Ïotta INt       IOTTA INT
    #  3                 Masha  &AMP;Lena Ltd. (Spb)    MASHA & LENA
    #  4                            bla-bla-bla Ltd.     BLA BLA BLA


# References

Magerman, T., Looy, V., Bart, & Song, X. (2006). *Data Production Methods for Harmonized Patent Statistics: Patentee Name Harmonization* (SSRN Scholarly Paper No. ID 944470). Rochester, NY: Social Science Research Network. Retrieved from <http://papers.ssrn.com/abstract=944470>

Thoma, G., Torrisi, S., Gambardella, A., Guellec, D., Hall, B. H., & Harhoff, D. (2010). Harmonizing and combining large datasets - an application to firm-level patent and accounting data. *National Bureau of Economic Research Working Paper Series*, (15851). Retrieved from <http://www.nber.org/papers/w15851> <http://www.nber.org/papers/w15851.pdf>

