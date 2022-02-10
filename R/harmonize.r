## -------->>  [[file:../harmonizer.src.org::*harmonize][harmonize:1]]
##' Makes list of procedures calls from table.
##'
##' Table should have at least two columns - messages and fuctions calls. Each function call should be a string of the following format "'function.name', arg1 = val1, arg2 = val2" (same as arguments for `do.call` function).
##' 
##' @param procedures_table Table to use
##' @param message_field name of the column with messages that will be displayed when each call is executed
##' @param function_call_field name of the column where function (harmonization procedures) calls are listed.
##' @param no.field name of the column where the number of procedure is specified. Also this field indicates if the row in the table is just a comment in which case it will be removed if `remove_comments` is set (which is set by default)
##' @param remove_comments Whether to remove comments.
##' 
##' @return List of named function calls. Names are messages.
##' 
harmonize_make_procedures_list <- function(procedures_table
                                         , message_field = "message"
                                         , function_call_field = "function.call"
                                         , no.field = "no"
                                         , remove_comments = TRUE
                                         , sort_by_no_field = TRUE
                                         , comments = c("#", "-", "")) {
    procedures_table <- defactor(procedures_table)
    if(remove_comments) {
        procedures_table <- 
            procedures_table[
                !(procedures_table[[no.field]] %in% comments)
              , ]
    }
    if(sort_by_no_field) {
        procedures_table <-
            procedures_table[
                procedures_table[[no.field]] |>
                as.numeric() |>
                order()
              , ]
    }
    procedures <- 
        procedures_table[[function_call_field]] |> (
            \(y) paste0("list(", y, ")")
        )() |>
        lapply(\(str) eval(parse(text = str))) |>
        lapply(\(lst) if(length(lst) == 1) unlist(lst) else lst)
    names(procedures) <- procedures_table[[message_field]]
    return(procedures)
}
## --------<<  harmonize:1 ends here



## -------->>  [[file:../harmonizer.src.org::*harmonize][harmonize:2]]
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
## --------<<  harmonize:2 ends here


