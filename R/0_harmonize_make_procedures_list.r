## -------->>  [[file:../harmonizer.src.org::harmonize_make_procedures_list][harmonize_make_procedures_list]]
##' Makes list of procedures calls from table.
##'
##' Table should have at least two columns - messages and fuctions
##' calls. Each function call should be a string of the following
##' format "'function.name', arg1 = val1, arg2 = val2" (same as
##' arguments for `do.call` function).
##' 
##' @param procedures_table Table to use
##' @param message_field name of the column with messages that will be
##'     displayed when each call is executed
##' @param function_call_field name of the column where function
##'     (harmonization procedures) calls are listed.
##' @param no_field name of the column where the number of procedure
##'     is specified. Also this field indicates if the row in the
##'     table is just a comment in which case it will be removed if
##'     `remove_comments` is set (which is set by default)
##' @param remove_comments Whether to remove comments.
##' @param sort_by_no_field Whether to sort the list by col named
##'     `no_field`
##' @param comments Values (character string) in the first col that
##'     makes entire row as commented out
##' @return List of named function calls. Names are messages.
##' 
harmonize_make_procedures_list <- function(procedures_table
                                         , message_field = "message"
                                         , function_call_field = "function.call"
                                         , no_field = "no"
                                         , remove_comments = TRUE
                                         , sort_by_no_field = TRUE
                                         , comments = c("#", "-", "")) {
    ## procedures_table <- defactor(procedures_table)
    if(remove_comments) {
        procedures_table <- 
            procedures_table[
                !(procedures_table[[no_field]] %in% comments)
              , ]
    }
    if(sort_by_no_field) {
        procedures_table <-
            procedures_table[
                procedures_table[[no_field]] |>
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
## --------<<  harmonize_make_procedures_list ends here


