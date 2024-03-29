## -------->>  [[file:../nstandr.src.org::*Package documentation][Package documentation:1]]
#' @details
#' (Organizational) Names STANDardization in R. Reproduces procedures described in Thoma et al. (2010), Magerman et al. (2006), Cockburn et al. (2009), Wasi & Flaaen (2015) and more.
#' @keywords internal
"_PACKAGE"
## --------<<  Package documentation:1 ends here



## -------->>  [[file:../nstandr.src.org::*standardize_toupper][standardize_toupper:1]]
##' @eval attr(standardize_toupper, "description")
##' 
##' @param x data
##' 
##' @inheritDotParams standardize_options
##'
##' @return updated data (as data.table)
##' @export
standardize_toupper <- function(x, ...) {
    get_target(x) |>
        toupper() |>
        inset_target(x)
}

attr(standardize_toupper, "description") <- 
"Uppercases vector of interest in the object (table)"
## --------<<  standardize_toupper:1 ends here



## -------->>  [[file:../nstandr.src.org::*standardize_remove_brackets][standardize_remove_brackets:1]]
##' @eval attr(standardize_remove_brackets, "@title")
##' @param x object (table)
##' @inheritDotParams standardize_options
##' @return updated object
##' 
##' @export
standardize_remove_brackets  <- function(x, ...) {
    get_target(x) |>
        stringi::stri_replace_all_regex("<[^<>]*>|\\([^()]*\\)|\\{[^{}]*\\}|\\[[^\\[\\]]*\\]", "") |>
        inset_target(x)
}

attr(standardize_remove_brackets, "@title") <- "Removes brackets and content in brackets"
## --------<<  standardize_remove_brackets:1 ends here



## -------->>  [[file:../nstandr.src.org::*standardize_remove_quotes][standardize_remove_quotes:1]]
##' Removes double quotes
##' 
##' @param x an object
##' @inheritDotParams standardize_options
##' @return updated object
##' @export
standardize_remove_quotes <- function(x, ...) {
        get_target(x) |>
          stringi::stri_replace_all_regex("\"", "") |>
          inset_target(x)
}
## --------<<  standardize_remove_quotes:1 ends here



## -------->>  [[file:../nstandr.src.org::*standardize_dehtmlize][standardize_dehtmlize:1]]
#' Converts HTML characters to UTF-8
#'
#' The method is about 1/3 faster than htmlParse but it is still quite slow
#' @param x object (table)
#' @param as_single_string If set then collapse characters in the main column of the `x` (i.e., `x.col`) as to a single string. It will increase performance (at least for relatively short tables). Default is FALSE
#' @param as_single_string_sep delimiter for collapsed strings to uncollapse it later. Default is "#_|".
#' @param use_read_xml If set the it will parse XML. Default is FALSE which means it parses HTML
#' @inheritDotParams standardize_options
#' @return updated object
#' @references http://stackoverflow.com/questions/5060076
#'
#' @export
standardize_dehtmlize <- function(x
                              , as_single_string = FALSE
                              , as_single_string_sep = "#_|"
                              , use_read_xml = FALSE
                              , ...) {
    x_vector <- get_target(x)
    if(as_single_string) {
        x_vector <- paste0(x_vector, collapse = as_single_string_sep)
        x_vector <- paste0("<x>", x_vector, "</x>")
        x_vector <- 
            (if(use_read_xml) {
                 xml2::read_xml(x_vector)
             } else {
                 xml2::read_html(x_vector)
             }) |> xml2::xml_text()
        strsplit(x_vector, as_single_string_sep, fixed = TRUE)[[1]]
    } else {
        sapply(x_vector, \(str) {
            str <- paste0("<x>", str, "</x>")
            (if(use_read_xml) {
                 xml2::read_xml(str)
             } else {
                 xml2::read_html(str)
             }) |> xml2::xml_text()
        }, USE.NAMES = FALSE)    
    } |> inset_target(x)
}
## --------<<  standardize_dehtmlize:1 ends here



## -------->>  [[file:../nstandr.src.org::*standardize_detect_enc][standardize_detect_enc:1]]
#' Detects string encoding
#' @param x object
#' @param output_codes_col_name Same as in [detect_patterns()]
#' @param return_only_codes Same as in [detect_patterns()]
#' @param ... 
#' @inheritDotParams standardize_options
#' @return updated object
#'
#' @export
standardize_detect_enc <- function(x
                               , output_codes_col_name = "{col_name_}encoding"
                               , return_only_codes = FALSE
                               , ...) {
    available_enc_list <- iconvlist()
    x_vector <- get_target(x) |>
        stringi::stri_enc_detect() |>
        lapply(function(enc) {
            enc <- toupper(enc[["Encoding"]])
            first_ok_enc <- which(enc %in% available_enc_list)[1]
            if(length(first_ok_enc) == 0) ""
            else enc[[first_ok_enc]]
        }) |> unlist()
    if(return_only_codes) {
        x_vector
    } else {
        inset_target(x_vector
                   , x
                   , output_placement = "omit"
                   , output_copy_col_name = output_codes_col_name
                   , append_output_copy = TRUE)
    }
}
## --------<<  standardize_detect_enc:1 ends here



## -------->>  [[file:../nstandr.src.org::*standardize_toascii][standardize_toascii:1]]
#' Translates non-ascii symbols to its ascii equivalent
#' 
#' @param x String to translate
#' @param detect_encoding Detect encoding of individual elements (slower). Allows to work with mixed encodings.
#' @inheritDotParams standardize_options
#' 
#' @export
standardize_toascii <- function(x
                            , detect_encoding = FALSE
                            , ...) {
  str <- get_target(x)
  utf <- nstandr_patterns_ascii$utf |> paste(collapse = "")
  ascii <- nstandr_patterns_ascii$ascii |> paste(collapse = "")
  (if(detect_encoding) {
       mapply(
           \(name, enc) chartr(utf, ascii, iconv(name, from = enc, to = "UTF-8", sub = ""))
         , str
         , standardize_detect_enc(str, return_only_codes = TRUE)
         , SIMPLIFY = FALSE, USE.NAMES = FALSE) |>
           unlist() |>
           iconv(to = "ASCII", sub = "")
   } else {
       ## stringi::stri_enc_toascii(str)
       chartr(utf, ascii, enc2utf8(str)) |> 
           iconv(to = "ASCII", sub = "")
   }) |> inset_target(x)
}
## --------<<  standardize_toascii:1 ends here



## -------->>  [[file:../nstandr.src.org::*standardize_x_split][standardize_x_split:1]]
##' Splits the object (table) in chunks by rows
##'
##' Convenient to apply some function to the table in chunks, e.g., if you want to add display of progress.
##'
##' @param x object or table
##' @param by number of rows to split by
##' @param len length of the table (nrow). If it is NULL then use x_length(x)
##' 
##' @return List of (sub)tables
standardize_x_split <- function(x, by, len = NULL) {
    if(is.null(len)) len <- x_length(x)
    split(x, rep(seq(1, len %/% by +1)
               , each = by
               , length.out = len))
}
## --------<<  standardize_x_split:1 ends here



## -------->>  [[file:../nstandr.src.org::*standardize][standardize:2]]
#' Standardizes organizational names. Takes either vector or column in the table.
#' 
#' @param x object (table)
#' @param procedures The procedures that basically comprise the standardization algorithm are specified as a list of either (1) names of procedures function as character strings or as (2) calls where you can provide optional arguments or (3) nested lists that allow user to group procedures. Nesting lists of procedures has an effect on standardization progress reporting and on visualizing algorithms with `nstandr_plot`. Technically nested lists are equivalent to plain list of procedures as it should produce same results. Names of the list elements are used for progress messages. For unnamed elements the name of procedure's function will be used for standardization progress reporting. Default is `nstandr:::nstandr_default_procedures_list`.
#' @param show_progress Whether to report progress percentage. Default is TRUE
#' @param nrows_min_to_show_progress The minimum number of rows the x should have for automatic progress estimation. If x has less rows no progress will be shown. Default is 10^5
#' @param progress_step_nrows If set it will divide the x into chunk of this amount of rows. Default is NULL.
#' @param progress_step_in_percent Number of percents that represent one step in progress. Value should be between 0.1 and 50. Default is 1 which means it will try to chunk the x into 100 pieces.
#' @param progress_message_use_names Should we use names from `procedures` list to report progress. Default is TRUE.
#' @param quite Suppress all messages. Default is FALSE.
#' @param save_intermediate_x_to_var For debuging of standartization procedures. Saves intermediate results to this variable. If procedures finish without errors then the variable will be removed.
#' @inheritDotParams standardize_options
#'
#' @aliases make_standard_names make_std_names nstand
#' 
#' @return standardized data
#'
#' @export
standardize <- function(x
                      , procedures = nstandr_default_procedures_list
                      , show_progress = TRUE
                      , nrows_min_to_show_progress = 10^3
                      , progress_step_nrows = NULL
                      , progress_step_in_percent = 1
                      , progress_message_use_names = TRUE
                      , quite = FALSE
                      , save_intermediate_x_to_var = NULL
                      , ...) {
    checkmate::assert_string(save_intermediate_x_to_var, null.ok = TRUE)
    checkmate::assert_flag(show_progress)
    checkmate::assert_flag(quite)
    checkmate::assert_flag(progress_message_use_names)
    checkmate::assert_class(procedures, classes = c("list"))
    ## make format of the massages for procedures
    message_delimiter <- paste(c("\n", rep("-", 65)), collapse = "")
    message_init <- paste0("\n* Applying standardization procedures:", message_delimiter)
    message_done  <- "\b\b\b\bDONE"
    progress_format <- "\b\b\b\b%3.0f%%"
    message_format <- "  - %-70.70s...."
    message_fin <- paste0("* Standardization is done!", message_delimiter)
    ## ensure that x is either vector or data.table
    if(missing(x)) return(NULL)
    checkmate::assert_multi_class(x
                                , classes = c("character"
                                            , "numeric"
                                            , "integer"
                                            , "logical"
                                            , "data.frame"
                                            , "data.table"))
    ## check progress_step_in_percent
    checkmate::assert_number(progress_step_in_percent, lower = 0.1, upper = 50)
    checkmate::assert_number(progress_step_nrows
                           , lower = x_length(x)/1000
                           , upper = x_length(x)/2
                           , null.ok = TRUE)
    x_len <- x_length(x)
    ## Set progress_step_nrows
    progress_step_nrows <-
        if (show_progress && !quite) {
            if(x_len < nrows_min_to_show_progress) {
                NULL
            } else if(!is.null(progress_step_nrows)) {
                progress_step_nrows
            } else {
                round(x_len / (100 / progress_step_in_percent))
            }
        }
    ## Apply Procedures
    parent_env <- parent.frame()
    x_env <- environment()
    for_procedures <- function(procedures) {
        for(p in 1:length(procedures)) {
            ## check if procedures[[p]] is nested
            if(is.list(procedures[[p]])) {
                if(!quite) message("* Starting '", names(procedures)[p], "' group of procedures...")
                for_procedures(procedures[[p]])
                if(!quite) message("  - Finished '", names(procedures)[p], "' group of procedures.")
                next
            }
            ## assume that procedures[[p]] is not list but could be a call
            ## get procedure function
            procedure_fun <- procedures[[p]][[1]] |>
                as.character()
            ## get procedure arguments (remove show_progressarg if it is there)
            procedure_args <- as.list(procedures[[p]])[
                -c(1, which(names(procedures[[p]]) == "show_progress"))
            ]
            ## get procedure names
            procedure_name <- 
                if(standardize_is_data_empty(names(procedures)[p], return_as_true_if_x_zero_length = TRUE) | !progress_message_use_names) {
                    procedure_fun
                } else {
                    names(procedures)[p]
                }
            ## Anounce Procedure Name
            if(!quite) packageStartupMessage(
                           sprintf(message_format, procedure_name)
                         , appendLF = FALSE)
            ## Check if we need report progress:
            ## progress is set & progress = FALSE is absent in the arguments
            if(!is.null(progress_step_nrows) &
               !isFALSE(procedures[[p]]["show_progress"][[TRUE]])) {
                ## check if we need to split..
                if(!isTRUE(class(x) == "list")) {
                    assign("x", value = standardize_x_split(x, progress_step_nrows), pos = x_env)
                }
                ## set progress counter
                i <- 0; i_env <- environment()
                ## Apply procedure to list!
                assign("x", value = 
                                lapply(x, \(x_by) {
                                    if(!is.null(save_intermediate_x_to_var))
                                        assign(save_intermediate_x_to_var, x, pos = 1)
                                    ## apply procedure fun with args
                                    x_by <- do.call(procedure_fun
                                                  , c(list(x_by), procedure_args)
                                                  , envir = parent_env)
                                    ## Increment progress counter
                                    assign("i", i + 100 * progress_step_nrows / x_len, pos = i_env)
                                    ## Anounce progress
                                    packageStartupMessage(
                                        sprintf(progress_format, i)
                                      , appendLF = FALSE)
                                    return(x_by)
                                })
                     , pos = x_env)
            } else {
                ## check if we need to rbindlist..
                if(isTRUE(class(x) == "list")) {
                    if(is.atomic(x[[1]])) {
                        assign("x", value = unlist(x, use.names = FALSE), pos = x_env)
                    } else {
                        assign("x", value = rbindlist(x), pos = x_env)
                    }
                }
                if(!is.null(save_intermediate_x_to_var))
                    assign(save_intermediate_x_to_var, x, pos = 1)
                ## Apply procedure fun with args!
                assign("x"
                     , value = do.call(procedure_fun
                                     , c(list(x), procedure_args)
                                     , envir = parent_env)
                     , pos = x_env)
            }
            ## Anounce DONE
            if(!quite) packageStartupMessage(message_done)
        }
    }
    if(!quite) message(message_init)
    for_procedures(procedures)
    if(!quite) message(message_fin)
    ## return x
    if(isTRUE(class(x) == "list")) {
        if(is.atomic(x[[1]])) {
            x <- unlist(x, use.names = FALSE)
        } else {
            x <- rbindlist(x)
        }
    }
    ## remove intermediate saves if procedures finished without error
    if(!is.null(save_intermediate_x_to_var))
        rm(list = save_intermediate_x_to_var, pos = 1)
    return(x)
}


## Alias

#' @rdname standardize
#' @export
make_std_names <- standardize


#' @rdname standardize
#' @export
make_standard_names <- standardize


#' @rdname standardize
#' @export
nstand <- standardize
## --------<<  standardize:2 ends here


