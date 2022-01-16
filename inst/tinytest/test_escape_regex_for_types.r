## -------->>  [[file:../../harmonizer.src.org::*harmonize_escape_regex][harmonize_escape_regex:2]]
escape_regex_for_types <- harmonizer:::escape_regex_for_types

expect_equal(
    c("MSlab$", "TriloBit.?", "(^0-3)", "Ltd.", "lalala") |>
    escape_regex_for_types(c("regex", "fixed", "regex", "ends", "trim_exact"), escape_fixed = FALSE)
  , c(`MSlab$` = "MSlab$", `TriloBit.?` = "TriloBit\\.\\?", `(^0-3)` = "(^0-3)", Ltd. = "Ltd\\.$", lalala = "^\\s*lalala\\s*$")
)

expect_equal(
    c("MSlab$", "TriloBit.?", "(^0-3)", "Ltd.", "lalala") |>
    escape_regex_for_types(c("regex", "fixed", "regex", "ends", "trim_exact"))
  , c(`MSlab$` = "MSlab$", `TriloBit.?` = "TriloBit\\.\\?", `(^0-3)` = "(^0-3)", 
      Ltd. = "Ltd\\.$", lalala = "^\\s*lalala\\s*$")
)
## --------<<  harmonize_escape_regex:2 ends here


