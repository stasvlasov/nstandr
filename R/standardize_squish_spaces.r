## -------->>  [[file:../harmonizer.src.org::*standardize_squish_spaces][standardize_squish_spaces:1]]
#' Removes redundant whitespases
#' @param x table or vector
#'
#' @param wrap_in_spaces If set then adds leaing and ending spaces. Default is FALSE.
#'
#' @inheritDotParams standardize_options
#'
#' @return updated table or vector
#' @export
standardize_squish_spaces <- function(x, wrap_in_spaces = FALSE, ...) {
    get_target(x) |>
        stringi::stri_replace_all_regex("\\s+", " ") |>
        stringi::stri_trim_both() |> (
            \(y) if(wrap_in_spaces) paste0(" ", y, " ") else y
        )() |>
        inset_target(x)
}
## --------<<  standardize_squish_spaces:1 ends here


