## -------->>  [[file:../../harmonizer.src.org::*standardize_make_procedures_list][standardize_make_procedures_list:2]]
standardize_make_procedures_list <- nstandr:::standardize_make_procedures_list

expect_equal(data.frame(no = c(3,2,"-", "")
                      , message = c("hello", "world", "man", "dfsdf")
                      , function.call = c("'c', 1, b=3", "'sum', 8,8,9", "'version'", "")) |>
             standardize_make_procedures_list()
           , list(world = list("sum", 8, 8, 9), hello = list("c", 1, b = 3)))



expect_equal(standardize_make_procedures_list(nstandr:::magerman_procedures_table)
           , list(`Upper casing` = "standardize_toupper", `Cleaning spaces` = "standardize_squish_spaces", 
                  `Removing HTML codes` = "magerman_remove_html_codes", `Cleaning spaces (2)` = "standardize_squish_spaces", 
                  `Replacing SGML coded characters` = "magerman_replace_sgml_characters", 
                  `Replacing proprietary characters` = "magerman_replace_proprietary_characters", 
                  `Detecting Umlauts` = list("magerman_detect_umlaut", output_codes_col_name = "magerman_umlaut"), 
                  `Replacing accented characters` = "magerman_replace_accented_characters", 
                  `Removing special characters` = "magerman_remove_special_characters", 
                  `Fixing quotation irregularities` = "magerman_remove_double_quotation_marks_irregularities", 
                  `Removing double quotations` = "magerman_remove_double_quotation_marks_beginning_end", 
                  `Removing non alphanumeric characters (1)` = "magerman_remove_non_alphanumeric_at_the_beginning", 
                  `Removing non alphanumeric characters (2)` = "magerman_remove_non_alphanumeric_at_the_end", 
                  `Fixing comma and period irregularities` = "magerman_replace_comma_period_irregularities", 
                  `Detecting legal form` = list("magerman_detect_legal_form", 
                                                output_codes_col_name = "legal_form"), `Removing legal form` = "magerman_remove_legal_form_and_clean", 
                  `Removing common words` = "magerman_remove_common_words", 
                  `Fixing spelling variations` = "magerman_replace_spelling_variation", 
                  Condensing = "magerman_condense", `Fixing umlaut variations` = list(
                                                        "magerman_replace_umlaut", has_umlaut_col = "magerman_umlaut", 
                                                        progress = FALSE)))
## --------<<  standardize_make_procedures_list:2 ends here


