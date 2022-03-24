## -------->>  [[file:../../harmonizer.src.org::*Umlaut Standardization][Umlaut Standardization:2]]
expect_equal(
    c("MÄKARÖNI ETÖ FKÜSNÖ Ltd"
             , "MSLab Co."
             , "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝŸ") |>
    magerman_detect_umlaut()
, structure(list(x = c("MÄKARÖNI ETÖ FKÜSNÖ Ltd", "MSLab Co.", 
"ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝŸ"
), x_has_umlaut = c(TRUE, FALSE, TRUE)), row.names = c(NA, -3L
), class = c("data.table", "data.frame"))
)


## test naming
expect_equal(c("MÄKARÖNI ETÖ FKÜSNÖ Ltd"
             , "MSLab Co."
             , "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝŸ") |>
             magerman_detect_umlaut(output_codes_col_name = "lala")
           , structure(list(x = c("MÄKARÖNI ETÖ FKÜSNÖ Ltd", "MSLab Co.", 
                                   "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝŸ"
                                   ), lala = c(TRUE, FALSE, TRUE)), row.names = c(NA, -3L), class = c("data.table", 
                                                                                                      "data.frame")))





expect_equal(data.frame(
    test = c("MÄKARÖNI ETÖ FKÜSNÖ Ltd"
           , "MSLab CÖ."
           , "MSLab Co."
           , "MSLaeb Comp."
           , "MSLab Comp."
           , "ÃÄÅÆÇÈÉÌÍÏÐÑÒÖØÚÝŸ") |> toupper()
  , log = "lot of coffee"
  , umlaut = c(TRUE, TRUE, FALSE, FALSE, FALSE, TRUE)) |>
  magerman_replace_umlaut(
      has_umlaut_col = "umlaut"
    , drop_has_umlaut_col = FALSE
    , replace_accented_characters = TRUE)
, structure(list(test = c("MAEKAEROENI ETOE FKUESNOE LTD", "MSLAEB COE.", 
                          "MSLAEB COE.", "MSLAEB COMP.", "MSLAB COMP.", "AEAEAEAECEEIIIÐNOEOEØUEYY"
                          ), log = c("lot of coffee", "lot of coffee", "lot of coffee", 
                                     "lot of coffee", "lot of coffee", "lot of coffee"), umlaut = c(TRUE, 
                                                                                                    TRUE, FALSE, FALSE, FALSE, TRUE)), row.names = c(NA, -6L), class = c("data.table", 
                                                                                                                                                                         "data.frame")))







expect_equal(data.frame(
    test = c("MÄKARÖNI ETÖ FKÜSNÖ Ltd"
           , "MSLab CÖ."
           , "MSLab Co."
           , "MSLaeb Comp."
           , "MSLab Comp."
           , "ÃÄÅÆÇÈÉÌÍÏÐÑÒÖØÚÝŸ") |> toupper()
  , log = "lot of coffee"
  , umlaut = c(TRUE, TRUE, FALSE, TRUE, FALSE, TRUE)) |>
  magerman_replace_umlaut(
      has_umlaut_col = "umlaut"
    , drop_has_umlaut_col = FALSE
    , replace_accented_characters = TRUE)
, structure(list(test = c("MAEKAEROENI ETOE FKUESNOE LTD", "MSLAEB COE.", 
                          "MSLAEB COE.", "MSLAEB COEMP.", "MSLAEB COEMP.", "AEAEAEAECEEIIIÐNOEOEØUEYY"
                          ), log = c("lot of coffee", "lot of coffee", "lot of coffee", 
                                     "lot of coffee", "lot of coffee", "lot of coffee"), umlaut = c(TRUE, 
                                                                                                    TRUE, FALSE, TRUE, FALSE, TRUE)), row.names = c(NA, -6L), class = c("data.table", 
                                                                                                                                                                        "data.frame")))








## test the col, rows args dependencies!
expect_equal(data.frame(
    log = "lot of coffee"
  , test = c("MÄKARÖNI ETÖ FKÜSNÖ Ltd"
           , "MSLab CÖ."
           , "MSLab Co."
           , "MSLaeb Comp."
           , "MSLab Comp."
           , "ÃÄÅÆÇÈÉÌÍÏÐÑÒÖØÚÝŸ") |> toupper()
  , umlaut = c(TRUE, TRUE, FALSE, TRUE, FALSE, TRUE)) |>
  magerman_replace_umlaut(
      has_umlaut_col = "umlaut"
    , drop_has_umlaut_col = FALSE
    , replace_accented_characters = TRUE
    , rows = c( FALSE, TRUE, FALSE, FALSE, FALSE, TRUE)
    , col = 2)
, structure(list(log = c("lot of coffee", "lot of coffee", "lot of coffee", 
                         "lot of coffee", "lot of coffee", "lot of coffee"), test = c("MÄKARÖNI ETÖ FKÜSNÖ LTD", 
                                                                                      "MSLAEB COE.", "MSLAB CO.", "MSLAEB COMP.", "MSLAB COMP.", "AEAEAEAECEEIIIÐNOEOEØUEYY"
                                                                                      ), umlaut = c(TRUE, TRUE, FALSE, TRUE, FALSE, TRUE)), row.names = c(NA, 
                                                                                                                                                          -6L), class = c("data.table", "data.frame")))
## --------<<  Umlaut Standardization:2 ends here


