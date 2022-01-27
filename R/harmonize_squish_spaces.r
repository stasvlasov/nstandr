## -------->>  [[file:../harmonizer.src.org::*harmonize_squish_spaces][harmonize_squish_spaces:1]]
#' Removes redundant whitespases
#' @param x table or vector
#'
#' @param wrap_in_spaces If set then adds leaing and ending spaces. Default is FALSE.
#'
#' @inheritDotParams harmonize_options
#'
#' @return updated table or vector
#' @export
harmonize_squish_spaces <- function(x, wrap_in_spaces = FALSE, ...) {
    get_target(x) |>
        stringi::stri_replace_all_regex("\\s+", " ") |>
        stringi::stri_trim_both() |> (
            \(y) if(wrap_in_spaces) paste0(" ", y, " ") else y
        )() |>
        inset_target(x)
}
## --------<<  harmonize_squish_spaces:1 ends here


