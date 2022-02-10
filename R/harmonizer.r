## -------->>  [[file:../harmonizer.src.org::*Package documentation][Package documentation:1]]
#' @details
#' Harmonizer package standardizes (harmonizes) organizational names
#'     mainly using procedures described in Thoma et al. (2010) and
#'     Magerman, Looy, Bart, & Song (2006) but not only.  This is work
#'     in progress. Please, file an issues or suggestion if you have
#'     any.  The main function is [harmonize()].
#' @keywords internal
"_PACKAGE"
## --------<<  Package documentation:1 ends here



## -------->>  [[file:../harmonizer.src.org::*harmonize_toupper][harmonize_toupper:1]]
##' @eval attr(harmonize_toupper, "description")
##' 
##' @param x data
##' 
##' @inheritDotParams harmonize_options
##'
##' @return updated data (as data.table)
##' @export
harmonize_toupper <- function(x, ...) {
    get_target(x) |>
        toupper() |>
        inset_target(x)
}

attr(harmonize_toupper, "description") <- 
"Uppercases vector of interest in the object (table)"
## --------<<  harmonize_toupper:1 ends here



## -------->>  [[file:../harmonizer.src.org::*harmonize_remove_brackets][harmonize_remove_brackets:1]]
##' @eval attr(harmonize_remove_brackets, "@title")
##' @param x object (table)
##' @inheritDotParams harmonize_options
##' @return updated object
##' 
##' @export
harmonize_remove_brackets  <- function(x, ...) {
    get_target(x) |>
        stri_replace_all_regex("<[^<>]*>|\\([^()]*\\)|\\{[^{}]*\\}|\\[[^\\[\\]]*\\]", "") |>
        inset_target(x)
}

attr(harmonize_remove_brackets, "@title") <- "Removes brackets and content in brackets"
## --------<<  harmonize_remove_brackets:1 ends here



## -------->>  [[file:../harmonizer.src.org::*harmonize_remove_quotes][harmonize_remove_quotes:1]]
##' Removes double quotes
##' 
##' @param x an object
##' @inheritDotParams harmonize_options
##' @return updated object
##' @export
harmonize_remove_quotes <- function(x, ...) {
        get_target(x) |>
          stri_replace_all_regex("\"", "") |>
          inset_target(x)
}
## --------<<  harmonize_remove_quotes:1 ends here



## -------->>  [[file:../harmonizer.src.org::*harmonize_dehtmlize][harmonize_dehtmlize:1]]
#' Converts HTML characters to UTF-8
#'
#' The method is about 1/3 faster than htmlParse but it is still quite slow
#' @param x object (table)
#' @param as_single_string If set then collapse characters in the main column of the `x` (i.e., `x.col`) as to a single string. It will increase performance (at least for relatively short tables). Default is FALSE
#' @param as_single_string_sep delimiter for collapsed strings to uncollapse it later. Default is "#_|".
#' @param read.xml If set the it will parse XML. Default is FALSE which means it parses HTML
#' @inheritDotParams harmonize_options
#' @return updated object
#' @references http://stackoverflow.com/questions/5060076
#'
#' @export
harmonize_dehtmlize <- function(x
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
## --------<<  harmonize_dehtmlize:1 ends here



## -------->>  [[file:../harmonizer.src.org::*harmonize_detect_enc][harmonize_detect_enc:1]]
#' Detects string encoding
#' @param x object
#' @param output_codes_col_name Same as in [detect_patterns()]
#' @param return_only_codes Same as in [detect_patterns()]
#' @param ... 
#' @inheritDotParams harmonize_options
#' @return updated object
#'
#' @export
harmonize_detect_enc <- function(x
                               , output_codes_col_name = "{col_name_}encoding"
                               , return_only_codes = FALSE
                               , ...) {
    available_enc_list <- iconvlist()
    x_vector <- get_target(x) |>
        stringi::stri_enc_detect() |>
        lapply(function(enc) {
            enc <- extract2(enc, "Encoding")
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
## --------<<  harmonize_detect_enc:1 ends here



## -------->>  [[file:../harmonizer.src.org::*harmonize_toascii][harmonize_toascii:1]]
#' Translates non-ascii symbols to its ascii equivalent
#' 
#' @param str String to translate
#' @param detect_encoding Detect encoding of individual elements (slower). Allows to work with mixed encodings.
#' @inheritDotParams harmonize_options
#' 
#' @export
harmonize_toascii <- function(x
                            , detect_encoding = FALSE
                            , ...) {
  str <- get_target(x)
  utf <- harmonizer_patterns_ascii$utf |> paste(collapse = "")
  ascii <- harmonizer_patterns_ascii$ascii |> paste(collapse = "")
  (if(detect_encoding) {
       mapply(
           \(name, enc) chartr(utf, ascii, iconv(name, from = enc, to = "UTF-8", sub = ""))
         , str
         , harmonize_detect_enc(str, return_only_codes = TRUE)
         , SIMPLIFY = FALSE, USE.NAMES = FALSE) |>
           unlist() |>
           iconv(to = "ASCII", sub = "")
   } else {
       chartr(utf, ascii, enc2utf8(str)) |> 
           iconv(to = "ASCII", sub = "")
   }) |> inset_target(x)
}
## --------<<  harmonize_toascii:1 ends here



## -------->>  [[file:../harmonizer.src.org::*harmonize_x_split][harmonize_x_split:1]]
##' Splits the object (table) in chunks by rows
##'
##' Convenient to apply some function to the table in chunks, e.g., if you want to add display of progress.
##'
##' @param x object or table
##' @param by number of rows to split by
##' @param len length of the table (nrow). If it is NULL then use x_length(x)
##' 
##' @return List of (sub)tables
harmonize_x_split <- function(x, by, len = NULL) {
    if(is.null(len)) len <- x_length(x)
    split(x, rep(seq(1, len %/% by +1)
               , each = by
               , length.out = len))
}
## --------<<  harmonize_x_split:1 ends here



## -------->>  [[file:../harmonizer.src.org::*harmonize][harmonize:1]]
##' Harmonizes organizational names. Takes either vector or column in the table.
##' 
##' @param x object (table)
##' @param procedures Named list of procedures (closures) to apply to x. If we need to pass arguments to some of the procedures it can be done by specifying sub-list where the first element is procedure and the rest its arguments. Names of the list elements are used for progress messages. Procedures can also be passed as data.frame in which case it will be converted to list of procedures with `harmonize_make_procedures_list` (see its help for the correct format of data.frame with procedures). Default is `harmonizer_default_procedures_table`
##' @param show_progress 
##' @param nrows_min_to_show_progress The minimum number of rows the x should have for automatic progress estimation. If x has less rows no progress will be shown. Default is 10^5
##' @param progress_step_nrows If set it will divide the x into chunk of this amount of rows. Default is NULL.
##' @param progress_step_in_percent Number of percents that represent one step in progress. Value should be between 0.1 and 50. Default is 1 which means it will try to chunk the x into 100 pieces.
##' @param progress_message_use_names Should we use names from `procedures` list to report progress. Default is TRUE.
##' @param quite Suppress all messages. Default is FALSE.
##' @param save_intermediate_x_to_var For debuging of standartization procedures. Saves intermediate results to this variable. If procedures finish without errors then the variable will be removed.
##' @param progress Show the progress? Default is TRUE
##' @inheritDotParams harmonize_options
##' 
##' @return
##'
##' @import stringi stringr magrittr
##' @export
harmonize <- function(x
                    , procedures = harmonizer_default_procedures_table
                    , show_progress = TRUE
                    , nrows_min_to_show_progress = 10^5
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
    checkmate::assert_multi_class(procedures, classes = c("list", "data.frame"))
    if(is.data.frame(procedures)) {
        procedures <- harmonize_make_procedures_list(procedures)
    }
    ## make format of the massages for procedures
    message_delimiter <- paste(c("\n", rep("-", 65), "\n"), collapse = "")
    message_init <- paste0("\nApplying harmonization procedures:", message_delimiter)
    message_done  <- "\b\b\b\bDONE"
    progress_format <- "\b\b\b\b%3.0f%%"
    message_format <- "* %-60.60s...."
    message_fin <- paste0(message_delimiter, "Harmonization is done!\n")
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
    if(!quite) message(message_init)
    for(p in 1:length(procedures)) {
        ## get procedure function
        procedure_fun <- procedures[[p]][[1]]
        ## get procedure arguments (remove show_progressarg if it is there)
        procedure_args <- procedures[[p]][
            -c(1, which(names(procedures[[p]]) == "show_progress"))
        ]
        ## get procedure names
        procedure_name <- 
            if(harmonize_is_data_empty(names(procedures)[p]) | !progress_message_use_names) {
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
                x <- harmonize_x_split(x, progress_step_nrows)
            }
            ## set progress counter
            i <- 0; env <- environment()
            ## Apply procedure to list!
            x <- lapply(x, \(x_by) {
                if(!is.null(save_intermediate_x_to_var))
                    assign(save_intermediate_x_to_var, x, pos = 1)
                ## apply procedure fun with args
                x_by <- do.call(procedure_fun
                              , c(list(x_by), procedure_args))
                ## Increment progress counter
                assign("i", i + 100 * progress_step_nrows / x_len, envir = env)
                ## Anounce progress
                packageStartupMessage(
                    sprintf(progress_format, i)
                  , appendLF = FALSE)
                return(x_by)
            })
        } else {
            ## check if we need to rbindlist..
            if(isTRUE(class(x) == "list")) {
                if(is.atomic(x[[1]])) {
                    x <- unlist(x, use.names = FALSE)
                } else {
                    x <- rbindlist(x)
                }
            }
            if(!is.null(save_intermediate_x_to_var))
                assign(save_intermediate_x_to_var, x, pos = 1)
            ## Apply procedure fun with args!
            x <- do.call(procedure_fun, c(list(x), procedure_args))
        }
        ## Anounce DONE
        if(!quite) packageStartupMessage(message_done)
    }
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
    remove
    if(!is.null(save_intermediate_x_to_var))
        rm(list = save_intermediate_x_to_var, pos = 1)
    return(x)
}
## --------<<  harmonize:1 ends here


