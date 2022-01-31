## -------->>  [[file:../harmonizer.src.org::*get_target & inset_target][get_target & inset_target:1]]
get_col_as_number <- function(col, x) {
      if(is.character(col)) {
          which(names(x) %in% col)
      } else {
          col
      }
  }

  infer_post_inset_col_from_pre_inset_col <- function(col, x, output_placement) {
      if(is.character(col)) {
          which(names(x) %in% col)
      } else {
          switch(
              output_placement
            , replace_col = col
            , append_to_col = col
            , prepend_to_col = col + 1
            , append_to_x = col
            , prepend_to_x = col + 1)
      }
  }

  infer_if_post_inset_col_possible <- function(col, x, output_placement) {
      col <- get_col_as_number(col, x)
      switch(
          output_placement
        , replace_col = TRUE
        , append_to_col = ifelse(col == x_width(x), FALSE, TRUE)
        , append_to_x = ifelse(col == x_width(x), FALSE, TRUE)
        , prepend_to_x = ifelse(col == 1, FALSE, TRUE)
        , prepend_to_col = ifelse(col == 1, FALSE, TRUE)
      )
  }


  infer_moving_target_from_post_inset_col <- function(col, x, output_placement, as_name = FALSE) {
      col <- get_col_as_number(col, x)
      return_col <- switch(
          output_placement
        , replace_col = col
        , append_to_col = col + 1
        , prepend_to_col = col - 1
        , append_to_x = x_width(x)
        , prepend_to_x = 1)
      if(as_name) {
          names(x)(return_col)
      } else {
          return_col
      }
  }


  ## this assumes that nothing else was never added...
  infer_moving_target_from_pre_inset_col <- function(col, x, output_placement, as_name = FALSE) {
      col <- get_col_as_number(col, x)
      return_col <- switch(
          output_placement
        , replace_col = col
        , append_to_col = col + 1
        , prepend_to_col = col
        , append_to_x = x_width(x)
        , prepend_to_x = 1)
      if(as_name) {
          names(x)(return_col)
      } else {
          return_col
      }
  }

## vars that come with dots
## col
## output_name
## output_placement
## assume that other stuff is always append to x or col so inference will keep working
infer_moving_target_from_names <- function(dots, x
                                         , return_null_for_new_col = FALSE
                                         , return_name_for_new_col = FALSE) {
    with(dots, {
        if(output_placement == "replace_col") return(get_col_as_number(col, x))
        col_post_inset <- infer_post_inset_col_from_pre_inset_col(col, x, output_placement)
        if(infer_if_post_inset_col_possible(col_post_inset, x, output_placement)) {
            target_name_generated <-
                  format_col_name(output_name, col_post_inset, x)
              if(target_name_generated %in% names(x)) {
                  ## case of subsequent calls
                  return(get_col_as_number(target_name_generated, x))
              }
        }
        if(return_null_for_new_col) return(NULL)
        col <- get_col_as_number(col, x)
        if(return_name_for_new_col) return(format_col_name(output_name, col, x))
        return(col)
    })
}




  ##' Gets a target vector to harmonize.
  ##'
  ##' @param data Input data. Can be vector, data.frame or a data.table
  ##' @param col Column of interest in the input `data`. The vector we would like to work with. This parameter is ignored if input `data` is a vector (checked by `is.atomic`)
  ##' @param rows Rows of interest
  ##' @param ... Ignored arguments that are meant for `inset_vector`
  ##' @return A vector. Factors in imput `data` are converted to string.
  ##'
  ##' @md
  get_target <- function(x, return_null_for_new_col = FALSE, ...) {
      with(dots <- get_harmonize_options(), {
          ## check arguments
          check_harmonize_options(dots, x)
          col <- infer_moving_target_from_names(dots, x, return_null_for_new_col)
          if(is.null(col)) return(NULL)
          get_vector(x, col, rows, check_x_col_rows = FALSE)
      })
  }



replace_fixed_if_string <- function(var, template = NULL) {
      var_name <- deparse1(substitute(var))
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
                            , target_col = NULL
                            , x_or_x_names = NULL
                            , return_docs = FALSE) {
      checkmate::assert_flag(return_docs)
      if(is.numeric(target_col)) {
          checkmate::assert_integerish(target_col
                                     , len = 1
                                     , lower = 1)
          checkmate::assert_multi_class(x_or_x_names, c("data.table", "data.frame"))
          x_names <- names(x_or_x_names)
          col_name <- x_names[[target_col]]
      } else {
          checkmate::assert_string(target_col
                                 , min.chars = 1
                                 , null.ok = TRUE
                                 , na.ok = TRUE)
          col_name <- target_col
          x_names <- x_or_x_names
      }
      procedure_index <- get0("harmonizer_harmonize_procedure_index", envir = parent.frame())
      procedure_name <- get0("harmonizer_harmonize_procedure_name", envir = parent.frame())
      replacements_list <- list(
          "procedure_index" = list(
              doc = ""
            , val = replace_fixed_if_string(procedure_index))
        , "procedure_name" = list(
              doc = ""
            , val = replace_fixed_if_string(procedure_name))
        , "_procedure_index" = list(
              doc = ""
            , val = replace_fixed_if_string(procedure_index, "_procedure_index"))
        , "_procedure_name" = list(
              doc = ""
            , val = replace_fixed_if_string(procedure_name, "_procedure_name"))
        , "_col_name" = list(
              doc = ""
            , val = replace_fixed_if_string(col_name, "_col_name"))
        , "col_name_" = list(
              doc = ""
            , val = replace_fixed_if_string(col_name, "col_name_"))
        , "col_name" = list(
              doc = ""
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



##' Insets target vector back to input object (`x`)
##' 
##' @param vector Character vector to inset into the `x` object
##' @param x Data to harmonize. Character vector or data.frame or
##'     data.table
##' @param omitted_rows_value_for_new_col Alternative value
##'     `omitted_rows_value` to use in case we create new column in
##'     x. For example, it is use in insetting codes to avoid the
##'     default `omitted_rows_value` use initial `col` in which case
##'     codes will be mixed with input values
##' @param allow_na_in_vector Whether to allow NA in inset vector
##' @param which_call_to_report System call number (e.g., -2L) to
##'     include in report if arguments checks fails
##' @param return_only_target_col If toggled to TRUE then only return
##'     the vector to be inset (output_placement argument is ignored)
##' @return Data.table or character vector
##' @inheritDotParams harmonize_options
inset_target <- function(vector, x
                       , omitted_rows_value_for_new_col = NULL
                       , allow_na_in_vector = TRUE
                       , which_call_to_report = -5L
                       , return_only_target_col = FALSE
                       , ...) {
    checkmate::assert_flag(allow_na_in_vector)
    checkmate::assert_flag(return_only_target_col)
    vector <- defactor_vector(vector)
    with(dots <- get_harmonize_options(), {
        ## check harmonize_options
        check_harmonize_options(dots, x)
        assertion_fails <- checkmate::makeAssertCollection()
        ## -----
        ## inset omitted_rows_value if needed
        ## -----
        checkmate::assert_multi_class(vector
                                    , classes = c("list", "character", "logical", "numeric")
                                    , add = assertion_fails)
        if(!is.null(rows)
           && ((is.logical(rows) && !all(rows))
               || (is.numeric(rows) && !setequal(rows, 1:x_length(x))))) {
            ## check vector lenth
            getFromNamespace(paste0("assert_", class(vector)), "checkmate")(
                vector
              , len = ifelse(is.numeric(rows), length(rows), sum(rows))
              , any.missing = allow_na_in_vector
              , add = assertion_fails
            )
            report_arg_checks(assertion_fails, which_call_to_report)
            ## process `omitted_rows_value`
            omitted_rows_value_col <-
                infer_moving_target_from_names(
                    dots
                  , x
                  , return_null_for_new_col =
                        !is.null(omitted_rows_value_for_new_col))
            if(is.null(omitted_rows_value_col) &&
               is.null(omitted_rows_value)) {
                omitted_rows_value <- omitted_rows_value_for_new_col
            }
            omitted_rows_value <-
                get_vector(x
                         , col = omitted_rows_value_col
                         , fallback_value = omitted_rows_value
                         , fallback_value_supersedes = TRUE
                         , check_x_col_rows = FALSE)
            ## inject ommited rows
            vector <- `[<-`(omitted_rows_value, rows, vector)
        } else {
            ## just check the vector length
            getFromNamespace(paste0("assert_", class(vector)), "checkmate")(
                vector
              , len = x_length(x)
              , any.missing = allow_na_in_vector
              , add = assertion_fails
            )
            report_arg_checks(assertion_fails, which_call_to_report)
            if(is.numeric(rows)) {
                ## case of permutations for same length
                vector <- vector[rows]
            }
        }
        ## -----
        ## inset full vector
        ## -----
        if(return_only_target_col) {
            x <- vector
        } else if(output_placement != "omit") {
            if(is.atomic(x) && output_placement == "replace_col") {
                ## just replace x if it is atomic
                x <- vector
            } else {
                x <- defactor(x, conv2dt = "all")
                width_pre_inset <- x_width(x)
                col_post_inset <- infer_post_inset_col_from_pre_inset_col(col, x, output_placement)
                col_or_name_if_new <-
                    infer_moving_target_from_names(dots, x, return_name_for_new_col = TRUE)
                ## fuckin data.table syntax is so cryptic
                ## [] at the end ensures that returned DT is printed
                x[, (col_or_name_if_new) := vector][]
                ## x[[col_or_name_if_new]] <- vector
                ## now if we added new col
                if(x_width(x) == width_pre_inset + 1) {
                    ## if new col was added place last col into target posision
                    target <- infer_moving_target_from_post_inset_col(col_post_inset, x, output_placement)
                    cols_nums <-
                        1:width_pre_inset |>
                        append(width_pre_inset + 1, after = target - 1)
                    data.table::setcolorder(x, cols_nums)
                }
            }
        }
        ## -----
        ## apped copy
        ## -----
        if(append_output_copy & !return_only_target_col) {
            x <- defactor(x, conv2dt = "all")
            col_post_inset <- infer_post_inset_col_from_pre_inset_col(col, x, output_placement)
            append_output_copy_name <- format_col_name(output_copy_col_name, col_post_inset, x)
            checkmate::assert_names(append_output_copy_name, add = assertion_fails)
            report_arg_checks(assertion_fails, which_call_to_report)
            ## [] at the end ensures that returned DT is printed
            x[, (append_output_copy_name) := vector][]
        }
        report_arg_checks(assertion_fails, which_call_to_report)
        return(x)
    })
}
## --------<<  get_target & inset_target:1 ends here


