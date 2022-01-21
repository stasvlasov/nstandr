## -------->>  [[file:../../harmonizer.src.org::*detect_patterns][detect_patterns:2]]
## Tests for detect_patterns
library("data.table")

## testing x.rows
expect_equal(
    detect_patterns(
        x = data.frame(
            name = c(
                "MSlab Co.",
                "IBM Corp.",
                "Tilburg University"
            ),
            codes = c("", 3, NA),
            lala = 1
        ),
        patterns = c("Co.", "Corp.", "MS"),
        patterns_type = "ends",
        codes_col_name = "codes.new",
        patterns_codes_col = 1,
        codes_merge = TRUE,
        rows = c(FALSE, TRUE, FALSE)
    ),
    data.table(name = c("MSlab Co.", "IBM Corp.", "Tilburg University"), codes = c("", "3", NA), lala = c(1, 1, 1), codes.new = c(
        NA,
        "Corp.", NA
    ))
)


## with codes.omitted.val = NA
expect_equal(
    detect_patterns(
        x = data.frame(
            name = c(
                "MSlab Co.",
                "IBM Corp.",
                "Tilburg University"
            ),
            codes = c("", 3, NA),
            lala = 1
        ),
        patterns = c("Co.", "Corp.", "MS"),
        patterns_type = "ends",
        codes_omitted_rows_value = "omitted",
        codes_col_name = "codes_new",
        patterns_codes_col = 1,
        codes_merge = TRUE,
        rows = c(FALSE, TRUE, FALSE)
    ),
    data.table(name = c("MSlab Co.", "IBM Corp.", "Tilburg University"), codes = c("", "3", NA), lala = c(1, 1, 1), codes_new = c("omitted", "Corp.", "omitted"))
)

## testing x.rows again with x.codes.col
expect_equal(
    detect_patterns(data.frame(
        name = c(
            "MSlab Co.",
            "IBM Corp.",
            "Tilburg University"
        ),
        codes = c("", 3, NA),
        lala = 1
    ),
    c("Co.", "Corp.", "MS"),
    patterns_type = "ends",
    patterns_codes_col = 1,
    codes_col_name = "codes",
    codes_merge = TRUE,
    rows = c(FALSE, TRUE, FALSE)
    ),
    structure(list(name = c("MSlab Co.", "IBM Corp.", "Tilburg University"), codes = list("", c("Corp.", "3"), NA_character_), lala = c(
        1,
        1, 1
    )), row.names = c(NA, -3L), class = c("data.table", "data.frame"))
)

## same as about but for vector
expect_equal(
    detect_patterns(c(
        "MSlab Co.",
        "IBM Corp.",
        "Tilburg University"
    ),
    patterns = c("Co.", "Corp.", "MS"),
    patterns_type = "ends",
    patterns_codes = "ala",
    return_only_codes = FALSE
    ),
    structure(list(V1 = c("MSlab Co.", "IBM Corp.", "Tilburg University"), V1_coded = c("ala", "ala", NA)), row.names = c(NA, -3L), class = c("data.table", "data.frame"))
)

expect_equal(
    detect_patterns(c(
        "MSlab Co.",
        "IBM Corp.",
        "Tilburg University"
    ),
    patterns = c("Co.", "Corp.", "MS"),
    patterns_type = "ends",
    patterns_codes = "ala",
    return_only_codes = TRUE
    ),
    c("ala", "ala", NA)
)




expect_equal(
    detect_patterns(
        data.frame(
            name = c(
                "MSlab Co.",
                "IBM Corp.",
                "Tilburg University"
            ),
            codes = c("", 3, NA)
        ),
        patterns = "Corp.",
        patterns_type = "ends",
        return_only_codes = TRUE
    ),
    c(NA, "Corp.", NA)
)






expect_equal(
    detect_patterns(c(
        "MSlab Co.",
        "IBM Corp.",
        "Tilburg University"
    ),
    data.table(c("Co.", "Co"),
        type = c("corp", "corp2"),
        some.extra.col = c(1, 2)
    ),
    return_only_first_detected_code = FALSE
    ),
    structure(list(V1 = c("MSlab Co.", "IBM Corp.", "Tilburg University"), V1_coded = list(c("corp", "corp2"), "corp2", character(0))), row.names = c(
        NA,
        -3L
    ), class = c("data.table", "data.frame"))
)





expect_equal(
    detect_patterns(c(
        "MSlab Co.",
        "IBM Corp.",
        "Tilburg University"
    ),
    patterns_codes = "single code",
    data.table(c("Co.", "Co"),
        some.extra.col = c(1, 2)
    )
    ),
    structure(list(V1 = c("MSlab Co.", "IBM Corp.", "Tilburg University"), V1_coded = list(
        c("single code", "single code"), "single code",
        character(0)
    )), row.names = c(NA, -3L), class = c(
        "data.table",
        "data.frame"
    ))
)

expect_equal(
    detect_patterns(c(
        "MSlab Co.",
        "IBM Corp.",
        "Tilburg University"
    ),
    data.table(c("Co.", "Co"),
        type = c(FALSE, TRUE),
        some.extra.col = c(1, 2)
    ),
    return_only_first_detected_code = TRUE
    ),
    structure(list(V1 = c("MSlab Co.", "IBM Corp.", "Tilburg University"), V1_coded = c("FALSE", "TRUE", NA)), row.names = c(NA, -3L), class = c(
        "data.table",
        "data.frame"
    ))
)




expect_equal(
    detect_patterns(c(
        "MSlab Co.",
        "IBM Corp.",
        "Tilburg University"
    ),
    data.frame(c("Co.", "Co"),
        type = c("corp", "corp2")
    ),
    return_only_first_detected_code = TRUE,
    patterns_type = "ends"
    ),
    structure(list(V1 = c("MSlab Co.", "IBM Corp.", "Tilburg University"), V1_coded = c("corp", NA, NA)), row.names = c(NA, -3L), class = c("data.table", "data.frame"))
)





expect_equal(
    detect_patterns(c(
        "MSlab Co",
        "MS3lab Co",
        "MSlab8 Co.",
        "IBM Corp.",
        "Tilburg University",
        " TiU    "
    ),
    data.frame(c("Co", "IBM", "MS[^0-9]+", "TiU", "Univ\\w+"),
        code = c("corp", "ibm", "ms", "tiu", "univ"),
        type = c("ends", "begins", "regex", "trim_exact", "regex")
    ),
    return_only_first_detected_code = TRUE,
    patterns_type_col = 3
    ),
    structure(list(V1 = c(
        "MSlab Co", "MS3lab Co", "MSlab8 Co.",
        "IBM Corp.", "Tilburg University", " TiU    "
    ), V1_coded = c(
        "corp",
        "corp", "ms", "ibm", "univ", NA
    )), row.names = c(NA, -6L), class = c(
        "data.table",
        "data.frame"
    ))
)





expect_equal(
    detect_patterns(c(
        "MSlab Co",
        "MS3lab Co",
        "MSlab8 Co.",
        "IBM Corp.",
        "Tilburg University",
        " TiU    "
    ),
    data.frame(c("Co", "IBM", "MS[^0-9]+", "TiU", "Univ\\w+"),
        code = c("corp", "ibm", "ms", "tiu", "univ"),
        type = c("ends", "begins", "regex", "trim_exact", "regex")
    ),
    patterns_type_col = 3
    ),
    structure(list(V1 = c(
        "MSlab Co", "MS3lab Co", "MSlab8 Co.",
        "IBM Corp.", "Tilburg University", " TiU    "
    ), V1_coded = list(
        c("corp", "ms"), "corp", "ms", "ibm", "univ", character(0)
    )), row.names = c(
        NA,
        -6L
    ), class = c("data.table", "data.frame"))
)

## testing adding to list

expect_equal(
    detect_patterns(data.table(
        name = c(
            "MSlab Co.",
            "IBM Corp.",
            "Tilburg University"
        ),
        codes = list(
            c("cool firm", "best firm ever"),
            "cool firm",
            "univer"
        ),
        lala = c(1, 2, 3)
    ),
    patterns = c("Co.", "Corp.", "MS"),
    patterns_type = "ends",
    patterns_codes = "corporation",
    codes_col_name = "codes",
    codes_merge = TRUE,
    rows = c(FALSE, TRUE, FALSE)
    ),
    data.table(name = c("MSlab Co.", "IBM Corp.", "Tilburg University"), codes = list(c("cool firm", "best firm ever"), c("corporation", "cool firm"), "univer"), lala = c(1, 2, 3))
)

## test all missmatches (all NAs call fall to logical class and cause errow on assesment)
expect_equal(
    detect_patterns(c(
        "MSlab Co.",
        "IBM Corp.",
        "Tilburg University"
    ),
    data.table(c("Coxx", "Corr"),
        type = c("corp", "corp2"),
        some.extra.col = c(1, 2)
    ),
    return_only_first_detected_code = FALSE
    ),
    structure(list(V1 = c("MSlab Co.", "IBM Corp.", "Tilburg University"), V1_coded = c(NA_character_, NA_character_, NA_character_)), row.names = c(
        NA,
        -3L
    ), class = c("data.table", "data.frame"))
)


## test unmatch codes
expect_equal(
    detect_patterns(c(
        "MSlab Co.",
        "IBM Corp.",
        "Tilburg University"
    ),
    data.table(c("Coxx", "Co."),
        type = c("corp", "corp2"),
        some.extra.col = c(1, 2)
    ),
    return_only_first_detected_code = FALSE,
    no_match_code = "no_match"
    ),
    structure(list(V1 = c("MSlab Co.", "IBM Corp.", "Tilburg University"), V1_coded = c("corp2", "no_match", "no_match")), row.names = c(NA, -3L), class = c("data.table", "data.frame"))
)


## test and_rows
expect_equal(data.table(a = c("MSlab Co."
                            , "IBM Corp."
                            , "Tilburg University Co.")
                      , a_coded = c(NA, "existing corp code", NA)) |>
             detect_patterns(
                 data.table(c("Corp.", "Co.", "Uni")
                          , type = c("corp", "corp2", "Uni")
                          , some.extra.col = c(1, 2, 3))
               , codes_update_empty = TRUE
               , return_only_first_detected_code = FALSE
               , rows = c( TRUE, TRUE, FALSE)
               , no_match_code = "no_match")
           , structure(list(a = c("MSlab Co.", "IBM Corp.", "Tilburg University Co."  ), a_coded = c("corp2", "existing corp code", NA)), row.names = c(NA, -3L), class = c("data.table", "data.frame")))
## --------<<  detect_patterns:2 ends here


