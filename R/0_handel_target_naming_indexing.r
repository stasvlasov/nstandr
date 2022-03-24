## -------->>  [[file:../harmonizer.src.org::*get_target & inset_target][get_target & inset_target:1]]
## naming col
make_indexed_col_name <- function(col_name
                                , x_names = NULL
                                , index_init_val = 1L
                                , index_separator = "_") {
    checkmate::assert_integer(index_init_val, len = 1)
    checkmate::assert_string(index_separator)
    if(is.null(x_names)) return(col_name)
    indexed_col_name_regex <- paste0("^", escape_regex(col_name), escape_regex(index_separator), "(\\d+)$")
    indexed_col_name_in_x_names <-
        stringi::stri_detect_regex(x_names, indexed_col_name_regex)
    if(any(indexed_col_name_in_x_names)) {
        index <- 
            stringi::stri_match_first_regex(x_names, indexed_col_name_regex)[,2] |>
            as.numeric() |>
            max(na.rm = TRUE)
        col_name <-
            paste0(col_name, index_separator, index + 1)
    } else if(col_name %in% x_names) {
        col_name <- 
            paste0(col_name, index_separator, index_init_val)
    } 
    return(col_name)
}

replace_fixed_if_string <- function(var, template = NULL) {
    var_name <- deparse1(substitute(var))
    var <- as.character(var)
    if(checkmate::test_string(var, min.chars = 1)) {
        if(is.null(template) || template == var) {
            return(var)
        } else {
            checkmate::assert_string(template, fixed = var_name)
            return(stringi::stri_replace_all_fixed(template, var_name, var))
        }
    } else {
        return("")
    }
}


format_col_name <- function(col_name_format
                          , col_name = NULL
                          , x_names = NULL
                          , return_docs = FALSE) {
    checkmate::assert_flag(return_docs)
    procedure_index <- get0("nstandr_standardize_procedure_index", envir = parent.frame())
    procedure_name <- get0("nstandr_standardize_procedure_name", envir = parent.frame())
    replacements_list <- list(
        "procedure_index" = list(
            doc = "replace '{procedure_index}' with a number of current standardization procedure is in the list of procedures"
          , val = replace_fixed_if_string(procedure_index))
      , "procedure_name" = list(
            doc = "replace '{procedure_name}' with name of current standardization procedure"
          , val = replace_fixed_if_string(procedure_name))
      , "_procedure_index" = list(
            doc = "replace '{_procedure_index}' with a number of current standardization procedure is in the list of procedures, prefixed with '_'"
          , val = replace_fixed_if_string(procedure_index, "_procedure_index"))
      , "_procedure_name" = list(
            doc = "replace '{procedure_name}' with name of current standardization procedure, prefixed with '_'"
          , val = replace_fixed_if_string(procedure_name, "_procedure_name"))
      , "_col_name" = list(
            doc = "replace '{_col_name}' with the name of 'col' argument (i.e., col with input value), prefixed with '_'"
          , val = replace_fixed_if_string(col_name, "_col_name"))
      , "col_name_" = list(
            doc = "replace '{col_name_}' with the name of 'col' argument (i.e., col with input value), suffixed with '_'"
          , val = replace_fixed_if_string(col_name, "col_name_"))
      , "col_name" = list(
            doc = "replace '{col_name}' with the name of 'col' argument (i.e., col with input value)"
          , val = replace_fixed_if_string(col_name))
      , "_index_suffix" = list(
            doc = "Adds unique suffix (increment index) if the column name already exist. E.g. the following format '{col_name}{_index_suffix}' for names(x) = c('V', 'V_9', 'V_41') and col = 2 (i.e. col_name = 'V_9' in this example) would result in a new column named 'V_42'"
          , val = "")
    )
    if(return_docs) {
        return(mapply(\(x, x_name) paste("*", x_name, "-", x$doc)
                    , replacements_list
                    , paste0("{", names(replacements_list), "}")))
    }
    ## check correctness of substitution names if provided
    replacements_in_col_name_format <- 
        stringi::stri_extract_all_regex(col_name_format
                                      , pattern = "\\{[^{}]*\\}"
                                      , omit_no_match = TRUE) |>
        unlist()
    if (length(replacements_in_col_name_format) > 0) {
        checkmate::assert_subset(replacements_in_col_name_format
                               , choices = paste0("{", names(replacements_list), "}")
                               , empty.ok = TRUE
                               , fmatch = TRUE
                               , .var.name = "*_col_name")
        col_name_formated <- 
            stringi::stri_replace_all_fixed(col_name_format
                                          , pattern = paste0("{", names(replacements_list), "}")
                                          , replacement = sapply(replacements_list, `[[`, "val")
                                          , vectorise_all = FALSE)
        if (stringi::stri_detect_fixed(col_name_format, "{_index_suffix}")) {
            ## ensure {_index_suffix} is at the end
            checkmate::assert_string(col_name_format, pattern = "\\{_index_suffix\\}$", .var.name = "*_col_name")
            col_name_formated <- make_indexed_col_name(col_name_formated, x_names)
        }
        return(col_name_formated)
    } else {
        return(col_name_format)
    }
}

get_col_as_name <- function(col, x_names) {
    if(is.numeric(col)) {
        x_names[col]
    } else {
        col
    }
}



## infering col number
get_col_as_number <- function(col, x_names) {
    if(is.character(col)) {
        which(x_names %in% col)
    } else {
        col
    }
}

infer_post_inset_col_from_pre_inset_col <- function(col, x_names, output_placement) {
    if(is.character(col)) {
        x_names %in% col
    } else {
        switch(
            output_placement
          , replace_col = col
          , append_to_col = col
          , prepend_to_col = col + 1
          , append_to_x = col
          , prepend_to_x = col + 1
          , omit = col)
    }
}

infer_if_post_inset_col_possible <- function(col, x_names, output_placement) {
    col <- get_col_as_number(col, x_names)
    switch(
        output_placement
      , replace_col = TRUE
      , append_to_col = ifelse(col == length(x_names), FALSE, TRUE)
      , append_to_x = ifelse(col == length(x_names), FALSE, TRUE)
      , prepend_to_x = ifelse(col == 1, FALSE, TRUE)
      , prepend_to_col = ifelse(col == 1, FALSE, TRUE)
      , omit = TRUE
    )
}


infer_moving_target_from_post_inset_col <- function(col, x_names, output_placement, as_name = FALSE) {
    col <- get_col_as_number(col, x_names)
    return_col <- switch(
        output_placement
      , replace_col = col
      , append_to_col = col + 1
      , prepend_to_col = col - 1
      , append_to_x = length(x_names)
      , prepend_to_x = 1
      , omit = NULL)
    if(as_name) {
        x_names[return_col]
    } else {
        return_col
    }
}


## this assumes that nothing else was never added...
infer_moving_target_from_pre_inset_col <- function(col, x_names, output_placement, as_name = FALSE) {
    col <- get_col_as_number(col, x_names)
    return_col <- switch(
        output_placement
      , replace_col = col
      , append_to_col = col + 1
      , prepend_to_col = col
      , append_to_x = length(x_names)
      , prepend_to_x = 1)
    if(as_name) {
        x_names[return_col]
    } else {
        return_col
    }
}


## assume that other stuff is always append to x or col so inference will keep working
infer_moving_target_from_names <- function(x_names
                                         , col
                                         , output_col_name
                                         , output_placement
                                         , return_null_for_new_col = FALSE
                                         , return_name_for_new_col = FALSE) {
    if(output_placement == "replace_col") {
        col_num <- get_col_as_number(col, x_names)
        return(col_num)
    }
    col_post_inset <- infer_post_inset_col_from_pre_inset_col(col, x_names, output_placement)
    if(infer_if_post_inset_col_possible(col_post_inset, x_names, output_placement)) {
        target_name_generated <-
            format_col_name(output_col_name, x_names[col_post_inset], x_names)
        if(target_name_generated %in% x_names) {
            ## case of subsequent calls
            col_num <- get_col_as_number(target_name_generated, x_names)
            return(col_num)
        }
    }
    if(return_null_for_new_col) return(NULL)
    if(return_name_for_new_col) {
        col_name <- get_col_as_name(col, x_names)
        col_name_formated <- format_col_name(output_col_name, col_name, x_names)
        return(col_name_formated)
    }
    col_num <- get_col_as_number(col, x_names)
    return(col_num)
}
## --------<<  get_target & inset_target:1 ends here


