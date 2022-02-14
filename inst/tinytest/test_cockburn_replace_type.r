## -------->>  [[file:../../harmonizer.src.org::*Identify Entity Type][Identify Entity Type:2]]
expect_equal(
    c(
        " DR VLASOV ",
        " S.VLASOV PHD ",
        " STANICA LEGALY REPRESENTED BY STAS",
        " DR VLASOV & BROTHER ",
        "MSlab & C",
        "LEGALY REPRESENTED BY STAS",
        " REPUBLIC LEGALY REPRESENTED BY STAS",
        " TILBURG UNIVERSTIY ",
        " VU UNIVERSTITAET ",
        " FUNDATION LEGALY REPRESENTED BY STAS"
    ) |>
        cockburn_detect_type(verbose = FALSE),
    structure(list(x = c(
        " DR VLASOV ", " S.VLASOV PHD ", " STANICA LEGALY REPRESENTED BY STAS",
        " DR VLASOV & BROTHER ", "MSlab & C", "LEGALY REPRESENTED BY STAS",
        " REPUBLIC LEGALY REPRESENTED BY STAS", " TILBURG UNIVERSTIY ",
        " VU UNIVERSTITAET ", " FUNDATION LEGALY REPRESENTED BY STAS"
    ), x_entity_type = list(
        "indiv", "indiv", c("hosp", "indiv"),
        c("indiv", "firm"), character(0), character(0), c(
            "govt",
            "indiv"
        ), "univ", "univ", c("inst", "indiv")
    )), row.names = c(
        NA,
        -10L
    ), class = c("data.table", "data.frame"))
)




expect_equal(
    data.table(
        name = c(
            "MÄKARÖNI ETÖ FKÜSNÖ Ltd",
            "MSLab CÖ. <a href=lsdldf> <br> <\\a>",
            "MSLab Co.",
            "MSLaeb Comp.",
            "MSLab Comp. Ltd.",
            "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝŸ  UNIVERSITY"
        ) |> rep(2),
        foo = "I love coffee"
    ) |>
        cockburn_detect_type(),
    structure(list(name = c(
        "MÄKARÖNI ETÖ FKÜSNÖ Ltd", "MSLab CÖ. <a href=lsdldf> <br> <\\a>",
        "MSLab Co.", "MSLaeb Comp.", "MSLab Comp. Ltd.", "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝŸ  UNIVERSITY",
        "MÄKARÖNI ETÖ FKÜSNÖ Ltd", "MSLab CÖ. <a href=lsdldf> <br> <\\a>",
        "MSLab Co.", "MSLaeb Comp.", "MSLab Comp. Ltd.", "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝŸ  UNIVERSITY"
    ), foo = c(
        "I love coffee", "I love coffee", "I love coffee",
        "I love coffee", "I love coffee", "I love coffee", "I love coffee",
        "I love coffee", "I love coffee", "I love coffee", "I love coffee",
        "I love coffee"
    ), name_entity_type = c(
        NA, NA, NA, NA, NA, "univ",
        NA, NA, NA, NA, NA, "univ"
    )), row.names = c(NA, -12L), class = c(
        "data.table",
        "data.frame"
    ))
)




expect_equal(
    c(
        "MÄKARÖNI ETÖ FKÜSNÖ Ltd",
        "MSLab CÖ. <a href=lsdldf> <br> <\\a>",
        "MSLab Co.",
        "MSLaeb Comp.",
        "MSLab Comp. Ltd.",
        "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝŸ UNIVERSITY"
    ) |> rep(2) |>
        cockburn_detect_type(),
    structure(list(x = c(
        "MÄKARÖNI ETÖ FKÜSNÖ Ltd", "MSLab CÖ. <a href=lsdldf> <br> <\\a>",
        "MSLab Co.", "MSLaeb Comp.", "MSLab Comp. Ltd.", "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝŸ UNIVERSITY",
        "MÄKARÖNI ETÖ FKÜSNÖ Ltd", "MSLab CÖ. <a href=lsdldf> <br> <\\a>",
        "MSLab Co.", "MSLaeb Comp.", "MSLab Comp. Ltd.", "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝŸ UNIVERSITY"
    ), x_entity_type = c(
        NA, NA, NA, NA, NA, "univ", NA, NA, NA,
        NA, NA, "univ"
    )), row.names = c(NA, -12L), class = c(
        "data.table",
        "data.frame"
    ))
)
## --------<<  Identify Entity Type:2 ends here


