# nstandr <img src="img/logo.png" align="right" alt="" width="120"/>

[![R-CMD-check](https://github.com/stasvlasov/nstandr/workflows/R-CMD-check/badge.svg)](https://github.com/stasvlasov/nstandr/actions)
[![codecov](https://codecov.io/gh/stasvlasov/nstandr/branch/master/graph/badge.svg?token=OQVJ7NRXO5)](https://codecov.io/gh/stasvlasov/nstandr)
![GitHub code size in bytes](https://img.shields.io/github/languages/code-size/stasvlasov/nstandr)

A package that does (Organizational) Names STANDardization in R.

`nstandr` reproduces procedures described in Thoma et al. (2010),
Magerman et al. (2006), Cockburn et al. (2009), Wasi & Flaaen (2015) and
more.

## Installation

``` r
devtools::install_github("stasvlasov/nstandr")
```

## Usage

The package provides its main function `standardize`. The function
expect character vector of organization names as input and returns its
standardized version.

For the standardization methods described in Magerman et al. (2006) and
Cockburn et al. (2009) you can use `standardize_magerman` and
`standardize_cockburn` respectively. These functions are similar to
`standardize(x, procedures=nstandr:::magerman_procedures_list))` and
`standardize(x, procedures=nstandr:::cockburn_procedures_list))` but
with additional options for tweaking original procedures and with more
documentation.

Here is an example of `standardize_magerman` usage

``` r
textConnection("SGS-THOMSON MICROELECTRONICS
S.G.S. THOMSON MICROELECTRONICS S.R.L.
S.G.S. THOMSON MICROELECTRONICS, S.R.L.
S.G.S.-THOMSON MICROELECTRONICS S.R.L.
SGS - THOMSON MICROELECTRONICS S.A.
SGS - THOMSON MICROELECTRONICS S.R.L.
SGS - THOMSON MICROELECTRONICS, INC.
SGS - THOMSON MICROELECTRONICS, S.R.L.
SGS THOMSON MICROELECTRONICS S.A.
SGS THOMSON MICROELECTRONICS S.R.L.
SGS THOMSON MICROELECTRONICS SA
SGS THOMSON MICROELECTRONICS SRL
SGS THOMSON MICROELECTRONICS, INC.
SGS THOMSON MICROELECTRONICS, S.A.
SGS- THOMSON MICROELECTRONICS, S.A.
SGS THOMSON MICROELECTRONICS, S.R.L.
SGS- THOMSON MICROELECTRONICS<BR>(PTE) LTD.
SGS THOMSON-MICROELECTRONICS SA
SGS-THOMSON MICROELECTRONIC S.A.
SGS-THOMSON MICROELECTRONICS
SGS-THOMSON MICROELECTRONICS GMBH
SGS-THOMSON MICROELECTRONICS INC.
SGS-THOMSON MICROELECTRONICS LIMITED
SGS-THOMSON MICROELECTRONICS LTD.
SGS-THOMSON MICROELECTRONICS PTE LTD
SGS-THOMSON MICROELECTRONICS PTE LTD.
SGS-THOMSON MICROELECTRONICS PTE. LIMITED
SGS-THOMSON MICROELECTRONICS PTE. LTD.
SGS-THOMSON MICROELECTRONICS S. R. L.
SGS-THOMSON MICROELECTRONICS S.A
SGS-THOMSON MICROELECTRONICS S.A.
SGS-THOMSON MICROELECTRONICS S.P.A.
SGS-THOMSON MICROELECTRONICS S.R. L.
SGS-THOMSON MICROELECTRONICS S.R.L
SGS-THOMSON MICROELECTRONICS S.R.L.
SGS--THOMSON MICROELECTRONICS S.R.L.
SGS-THOMSON MICROELECTRONICS SA
SGS-THOMSON MICROELECTRONICS SPA
SGS-THOMSON MICROELECTRONICS SRL
SGS-THOMSON MICROELECTRONICS SRL.
SGS-THOMSON MICROELECTRONICS, GMBH
SGS-THOMSON MICROELECTRONICS, INC
SGS-THOMSON MICROELECTRONICS, INC.
SGS-THOMSON MICROELECTRONICS, LTD.
SGS-THOMSON MICROELECTRONICS, PTE LTD.
SGS-THOMSON MICROELECTRONICS, S.A.
SGS-THOMSON MICROELECTRONICS, S.R.L.
SGS-THOMSON MICROELECTRONICS, S.RL
SGS-THOMSON MICROELECTRONICS, SA
SGS-THOMSON MICROELECTRONICS, SA.
SGS-THOMSON MICROELECTRONICS, SRL
SGS-THOMSON MICROELECTRONICS,S.R.L.") |>
    readLines() |>
    standardize_magerman(output_placement = "append_to_x")

# 
# Applying standardization procedures:
# -----------------------------------------------------------------
# 
# * Upper casing                                               DONE
# * Cleaning spaces                                            DONE
# * Removing HTML codes                                        DONE
# * Cleaning spaces (2)                                        DONE
# * Replacing SGML coded characters                            DONE
# * Replacing proprietary characters                           DONE
# * Detecting Umlauts                                          DONE
# * Replacing accented characters                              DONE
# * Removing special characters                                DONE
# * Fixing quotation irregularities                            DONE
# * Removing double quotations                                 DONE
# * Removing non alphanumeric characters (1)                   DONE
# * Removing non alphanumeric characters (2)                   DONE
# * Fixing comma and period irregularities                     DONE
# * Removing legal form                                        DONE
# * Removing common words                                      DONE
# * Fixing spelling variations                                 DONE
# * Condensing                                                 DONE
# * Fixing umlaut variations                                   DONE
# 
# -----------------------------------------------------------------
# Standardization is done!
# 
#                                               x                     std_x
#  1:                SGS-THOMSON MICROELECTRONICS SGSTHOMSONMICROELECTRONIC
#  2:      S.G.S. THOMSON MICROELECTRONICS S.R.L. SGSTHOMSONMICROELECTRONIC
#  3:     S.G.S. THOMSON MICROELECTRONICS, S.R.L. SGSTHOMSONMICROELECTRONIC
#  4:      S.G.S.-THOMSON MICROELECTRONICS S.R.L. SGSTHOMSONMICROELECTRONIC
#  5:         SGS - THOMSON MICROELECTRONICS S.A. SGSTHOMSONMICROELECTRONIC
#  6:       SGS - THOMSON MICROELECTRONICS S.R.L. SGSTHOMSONMICROELECTRONIC
#  7:        SGS - THOMSON MICROELECTRONICS, INC. SGSTHOMSONMICROELECTRONIC
#  8:      SGS - THOMSON MICROELECTRONICS, S.R.L. SGSTHOMSONMICROELECTRONIC
#  9:           SGS THOMSON MICROELECTRONICS S.A. SGSTHOMSONMICROELECTRONIC
# 10:         SGS THOMSON MICROELECTRONICS S.R.L. SGSTHOMSONMICROELECTRONIC
# 11:             SGS THOMSON MICROELECTRONICS SA SGSTHOMSONMICROELECTRONIC
# 12:            SGS THOMSON MICROELECTRONICS SRL SGSTHOMSONMICROELECTRONIC
# 13:          SGS THOMSON MICROELECTRONICS, INC. SGSTHOMSONMICROELECTRONIC
# 14:          SGS THOMSON MICROELECTRONICS, S.A. SGSTHOMSONMICROELECTRONIC
# 15:         SGS- THOMSON MICROELECTRONICS, S.A. SGSTHOMSONMICROELECTRONIC
# 16:        SGS THOMSON MICROELECTRONICS, S.R.L. SGSTHOMSONMICROELECTRONIC
# 17: SGS- THOMSON MICROELECTRONICS<BR>(PTE) LTD. SGSTHOMSONMICROELECTRONIC
# 18:             SGS THOMSON-MICROELECTRONICS SA SGSTHOMSONMICROELECTRONIC
# 19:            SGS-THOMSON MICROELECTRONIC S.A. SGSTHOMSONMICROELECTRONIC
# 20:                SGS-THOMSON MICROELECTRONICS SGSTHOMSONMICROELECTRONIC
# 21:           SGS-THOMSON MICROELECTRONICS GMBH SGSTHOMSONMICROELECTRONIC
# 22:           SGS-THOMSON MICROELECTRONICS INC. SGSTHOMSONMICROELECTRONIC
# 23:        SGS-THOMSON MICROELECTRONICS LIMITED SGSTHOMSONMICROELECTRONIC
# 24:           SGS-THOMSON MICROELECTRONICS LTD. SGSTHOMSONMICROELECTRONIC
# 25:        SGS-THOMSON MICROELECTRONICS PTE LTD SGSTHOMSONMICROELECTRONIC
# 26:       SGS-THOMSON MICROELECTRONICS PTE LTD. SGSTHOMSONMICROELECTRONIC
# 27:   SGS-THOMSON MICROELECTRONICS PTE. LIMITED SGSTHOMSONMICROELECTRONIC
# 28:      SGS-THOMSON MICROELECTRONICS PTE. LTD. SGSTHOMSONMICROELECTRONIC
# 29:       SGS-THOMSON MICROELECTRONICS S. R. L. SGSTHOMSONMICROELECTRONIC
# 30:            SGS-THOMSON MICROELECTRONICS S.A SGSTHOMSONMICROELECTRONIC
# 31:           SGS-THOMSON MICROELECTRONICS S.A. SGSTHOMSONMICROELECTRONIC
# 32:         SGS-THOMSON MICROELECTRONICS S.P.A. SGSTHOMSONMICROELECTRONIC
# 33:        SGS-THOMSON MICROELECTRONICS S.R. L. SGSTHOMSONMICROELECTRONIC
# 34:          SGS-THOMSON MICROELECTRONICS S.R.L SGSTHOMSONMICROELECTRONIC
# 35:         SGS-THOMSON MICROELECTRONICS S.R.L. SGSTHOMSONMICROELECTRONIC
# 36:        SGS--THOMSON MICROELECTRONICS S.R.L. SGSTHOMSONMICROELECTRONIC
# 37:             SGS-THOMSON MICROELECTRONICS SA SGSTHOMSONMICROELECTRONIC
# 38:            SGS-THOMSON MICROELECTRONICS SPA SGSTHOMSONMICROELECTRONIC
# 39:            SGS-THOMSON MICROELECTRONICS SRL SGSTHOMSONMICROELECTRONIC
# 40:           SGS-THOMSON MICROELECTRONICS SRL. SGSTHOMSONMICROELECTRONIC
# 41:          SGS-THOMSON MICROELECTRONICS, GMBH SGSTHOMSONMICROELECTRONIC
# 42:           SGS-THOMSON MICROELECTRONICS, INC SGSTHOMSONMICROELECTRONIC
# 43:          SGS-THOMSON MICROELECTRONICS, INC. SGSTHOMSONMICROELECTRONIC
# 44:          SGS-THOMSON MICROELECTRONICS, LTD. SGSTHOMSONMICROELECTRONIC
# 45:      SGS-THOMSON MICROELECTRONICS, PTE LTD. SGSTHOMSONMICROELECTRONIC
# 46:          SGS-THOMSON MICROELECTRONICS, S.A. SGSTHOMSONMICROELECTRONIC
# 47:        SGS-THOMSON MICROELECTRONICS, S.R.L. SGSTHOMSONMICROELECTRONIC
# 48:          SGS-THOMSON MICROELECTRONICS, S.RL SGSTHOMSONMICROELECTRONIC
# 49:            SGS-THOMSON MICROELECTRONICS, SA SGSTHOMSONMICROELECTRONIC
# 50:           SGS-THOMSON MICROELECTRONICS, SA. SGSTHOMSONMICROELECTRONIC
# 51:           SGS-THOMSON MICROELECTRONICS, SRL SGSTHOMSONMICROELECTRONIC
# 52:         SGS-THOMSON MICROELECTRONICS,S.R.L. SGSTHOMSONMICROELECTRONIC
#                                               x                     std_x
```

# References

Magerman, T., Looy, V., Bart, & Song, X. (2006). *Data Production
Methods for Harmonized Patent Statistics: Patentee Name Standardization*
(SSRN Scholarly Paper No. ID 944470). Rochester, NY: Social Science
Research Network. Retrieved from
<http://papers.ssrn.com/abstract=944470>

Thoma, G., Torrisi, S., Gambardella, A., Guellec, D., Hall, B. H., &
Harhoff, D. (2010). Harmonizing and combining large datasets - an
application to firm-level patent and accounting data. *National Bureau
of Economic Research Working Paper Series*, (15851). Retrieved from
<http://www.nber.org/papers/w15851>
<http://www.nber.org/papers/w15851.pdf>

Wasi, N., & Flaaen, A. (2015). Record linkage using Stata:
Preprocessing, linking, and reviewing utilities. The Stata Journal,
15(3), 672-697. Retrieved from
<https://ebp-projects.isr.umich.edu/NCRN/papers/wasi_flaaen_statarecordlinkageutilities.pdf>

## Dependencies

| name                            | version | comment                                   |
|---------------------------------|---------|-------------------------------------------|
| [R](https://www.r-project.org/) | 4.2.0   | minimum R version to enable native piping |

Hard dependencies (`Depends` field in `DESCRIPTION` file)

### Required packages

| name                                                   | version | comment                                                   |
|--------------------------------------------------------|---------|-----------------------------------------------------------|
| [data.table](https://rdatatable.gitlab.io/data.table/) |         | fast data.frames, used as main input and output data type |
| [stringi](https://stringi.gagolewski.com/)             |         | fast string manipulations                                 |
| [xml2](https://xml2.r-lib.org/)                        |         | cleaning web syntax                                       |
| [checkmate](https://mllg.github.io/checkmate/)         |         | function arguments checker, ensures stability             |

Required packages (`Imports` field in the `DESCRIPTION` file)

### Suggested packages

| name                                                                            | version | comment                                                                |
|---------------------------------------------------------------------------------|---------|------------------------------------------------------------------------|
| [tinytest](https://github.com/markvanderloo/tinytest/blob/master/pkg/README.md) |         | package development (unit testing)                                     |
| [fastmatch](https://cran.r-project.org/web/packages/fastmatch/index.html)       |         | can speed things up                                                    |
| [htmltools](https://rstudio.github.io/htmltools/index.html)                     |         | used for escaping html in procedures descriptions before visualization |
| [DiagrammeR](http://rich-iannone.github.io/DiagrammeR/docs.html)                |         | needed for visualizing procedures lists                                |

Suggested packages (`Suggests` field in the `DESCRIPTION` file)

### Development dependencies and tools

These packages are used for developing and building `nstandr`

| name                                                               | version | comment                       |
|--------------------------------------------------------------------|---------|-------------------------------|
| [devtools](https://devtools.r-lib.org/)                            |         | builds the package            |
| [roxygen2](https://roxygen2.r-lib.org/)                            |         | makes docs                    |
| [languageserver](https://github.com/REditorSupport/languageserver) |         | provides some IDE consistency |
| [usethis](https://usethis.r-lib.org/)                              |         | repo utils                    |
| [boomer](https://moodymudskipper.github.io/boomer)                 |         | can be used for debugging     |

Useful packages for development
