## -------->>  [[file:../harmonizer.src.org::*inset_vector][inset_vector:3]]
##' Insets target vector back to input object (`data`)
inset_vector <- function(vector, x, ...) {
    with(dots <- get_harmonize_options(), {
        ## check harmonize_options
        check_harmonize_options(dots, x)
        moving_target <- get_moving_target(dots, x)
        ## inset ommitted_rows_values if needed
        if(!is.null(rows)
           && ((is.logical(rows) && !all(rows))
               || (is.numeric(rows) && !setequal(rows, 1:x_length(x))))) {
            ## check vector lenth
            if(is.logical(rows)) {
                checkmate::assert_character(vector, len = sum(rows))
            } else if(is.numeric(rows)){
                checkmate::assert_character(vector, len = length(rows))
            }
            ## process `ommitted_rows_values`
            if(is.null(ommitted_rows_values)) {
                ommitted_rows_values <- harmonize_defactor(x[[moving_target]])
            }
            if(length(ommitted_rows_values) != x_length(x)) {
                ## assume `ommitted_rows_values` length 1
                ommitted_rows_values <- rep(ommitted_rows_values, x_length(x))
            }
            ## inject ommited rows
            vector_full <- ommitted_rows_values
            vector_full[rows] <- vector
            vector <- vector_full
        } else {
            ## just check the vector length
            checkmate::assert_character(vector, len = x_length(c))
            if(is.numeric(rows) && setequal(rows, 1:x_length(x))) {
                ## case of permutations for same length
                vector <- vector[rows]
            }
        }
        ## inset full vector
        ## for now assume that it is not the first fun
        ## fuckin data.table syntax is so cryptic
        x[, (moving_target) := vector]
    })
    return(x)
}
## --------<<  inset_vector:3 ends here


