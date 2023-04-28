## -------->>  [[file:../nstandr.src.org::*get_dots (get from external package)][get_dots (get from external package):1]]
##' Provides access to arguments of nested functions. Sort of an alterative mechanism to passing `...` arguments but with more features.
##'
##' Provides access to higher level call's arguments (including `...` dots arguments) without explicitly passing it through calling stack and allows updating default values that are explicitly set throughout calling stack (i.e., lower calls take prevalence).
##'
##' @param function_or_arg_list The end function that meant to accept dots arguments (default arguments accessed with `formals(function_or_arg_list)`) or just explicit list of default dots arguments that will be searched up in calling stack and updated if set explicitly in higher calls. If set to NULL then use formals of the parent call (assessed with `sys.function(-1L)`).
##' @param select_args Which arguments to select from `function_or_arg_list`. Ether character or numeric vector.
##' @param search_calls_with_formals Formals (parameters, arguments) that should be present in each upper call to continue looking up the call stack for updates in dots arguments.
##' @param search_calls_of_env Environment/package name (character string) to which each function in upper calls to should belong to continue looking up the call stack for updates in dots arguments.
##' @param search_calls_regexp Regular expression that each function name in upper calls to should matched to continue looking up the call stack for updates in dots arguments.
##' @param search_depth Number of frames (aka environments) down in calling stack to look up arguments.
##' @param search_up_to_call The name of the call before which to continue looking up the call stack for updates in dots arguments.
##' @param skip_checks_for_parent_call Whether to skip checking `search_calls_with_formals` `search_calls_of_env` `search_calls_regexp`
##' @param eval_default_args Whether to evaluate default arguments. Default is do not evaluate (FALSE) assuming that all argument are simple values (i.e., evaluates to itself)
##' @param return_unlisted_if_single_arg Toggle wether unlist when returning a single argument. Default is TRUE
##'
##' @return List of updated dots arguments
##' 
##' @examples
##' # Make get_dots available for following examples
##' get_dots <- nstandr:::get_dots
##' # Basic usage
##' util <- function(foo = 0, bar = 0) {
##'     # get dots and bind updated arguments into environment
##'     dots <- get_dots()
##'     for (v in names(dots)) assign(v, dots[[v]])
##'     # util just reports it arguments
##'     message("foo: ", foo, ", bar: ", bar)
##' }
##' 
##' util()
##' #> foo: 0, bar: 0
##' 
##' main <- function (...) {
##'     util()
##'     util(foo = 1) 
##'     util(bar = 1)
##' }
##' 
##' main(foo = 2, bar = 2)
##' #> foo: 2, bar: 2
##' #> foo: 1, bar: 2  # THIS WORKS NOW!
##' #> foo: 2, bar: 1  # THIS WORKS NOW!
##'
##' # Usage in nested calls
##' util <- function(foo = 0, bar = 0) {
##'     # get dots and bind updated arguments into environment
##'     dots <- get_dots(search_depth = 3L)
##'     for (v in names(dots)) assign(v, dots[[v]])
##'     # util just reports it arguments
##'     message("foo: ", foo, ", bar: ", bar)
##' }
##' 
##' main <- function (...) {
##'     util()
##'     sub_main(foo = 1)
##' }
##' 
##' sub_main <- function (...) {
##'     util()
##'     sub_sub_main(bar = 2)
##' }
##' 
##' sub_sub_main <- function (...) {
##'     util()
##' }
##' 
##' main()
##' #> foo: 0, bar: 0
##' #> foo: 1, bar: 0
##' #> foo: 0, bar: 2
get_dots <- function(function_or_arg_list = NULL
                   , select_args = NULL
                   , search_calls_with_formals = "..."
                   , search_calls_of_env = NULL
                   , search_calls_regexp = NULL
                   , search_depth = 1L
                   , search_up_to_call = NULL
                   , skip_checks_for_parent_call = TRUE
                   , eval_default_args = FALSE
                   , return_unlisted_if_single_arg = TRUE) {

    ## check arguments with checkmate (optionally)
    if (requireNamespace("checkmate", quietly = TRUE)) {
        checkmate::assert(checkmate::check_function(function_or_arg_list, null.ok = TRUE)
                        , checkmate::check_list(function_or_arg_list))
        checkmate::assert_character(select_args, null.ok = TRUE)
        checkmate::assert_character(search_calls_with_formals, null.ok = TRUE)
        checkmate::assert_character(search_calls_of_env, null.ok = TRUE)
        checkmate::assert_character(search_calls_regexp, null.ok = TRUE)
        checkmate::assert_integer(search_depth)
        checkmate::assert_character(search_up_to_call, null.ok = TRUE)
        checkmate::assert_flag(eval_default_args)
        checkmate::assert_flag(return_unlisted_if_single_arg)
    }
    ## set default_args
    if (is.null(function_or_arg_list)) {
        function_or_arg_list <- sys.function(-1L)
    }
    if (is.null(function_or_arg_list)) {
        stop("get_dots -- supposed to be called inside function (nframe >= 1)")
    }
    if (is.function(function_or_arg_list)) {
        default_args <- formals(function_or_arg_list) |> as.list()
    } else if(is.list(function_or_arg_list)) {
        default_args <- function_or_arg_list
    }
    ## filter ... argument out (just in case)
    default_args <- default_args[!(default_args %in% "...")]
    if (length(select_args) > 0) {
        default_args <- default_args[select_args]
        if (length(default_args) == 0) stop("get_dots -- 'select_args' are not in 'formals(function_or_arg_list)'")
    }
    ## eval default args just in case
    if(eval_default_args &&
       length(default_args) != 0 &&
       is.function(function_or_arg_list)) {
        default_args <-
            lapply(default_args, eval, environment(function_or_arg_list))
    }
    ## collect explicit args in parents
    explicit_args <- list()
    sp <- sys.parent()
    for (fr in unique(rev(sys.parents()))) {
        ## stop searching frames stack deeper than search_depth
        if (fr < 1 || (sp - fr) > search_depth) break()
        ## check if we are searching only in 'friendly' functions:
        ## meaning that at least search_calls_with_formals should exist in calls
        parent_fun <- sys.function(fr)
        parent_default_args <- formals(parent_fun) |> as.list()
        if (!(skip_checks_for_parent_call && fr == sp) &&
            !all(search_calls_with_formals %in% names(parent_default_args))) next()
        ## check if call belongs to an env (package)
        if (!(skip_checks_for_parent_call && fr == sp) &&
            !is.null(search_calls_of_env) &&
            !(environmentName(environment(parent_fun)) %in% search_calls_of_env)) next()
        ## check if call matches regexp
        parent_call <- sys.call(fr) |> as.list()
        if (!(skip_checks_for_parent_call && fr == sp) &&
            !is.null(search_calls_regexp) &&
            !grepl(search_calls_regexp, as.character(parent_call[[1]]), perl = TRUE)) next()
        ## update defautls other parent defaults
        if(any(default_args_update <-
                   names(parent_default_args) %in% names(default_args))) {
            default_args_update <- parent_default_args[default_args_update]
            if(eval_default_args) {
                default_args_update <- lapply(default_args_update, eval, environment(parent_fun))
            }
            default_args <- 
                c(default_args[!(names(default_args) %in% names(parent_default_args))]
                ,  default_args_update)
        }
        ## if explicit arg is in args list and not already added then add it
        parent_args <- parent_call[-1]
        if (length(parent_args) > 0) {
            args_to_add <-
                (names(parent_args) %in% names(default_args)) &
                !(names(parent_args) %in% names(explicit_args))
            if (any(args_to_add)) {
                ## evaluate args in the frame that was created before 'fr'
                ## the direct parent of 'fr' might be wrong environment to look for args
                args_to_add <- 
                    lapply(parent_args[args_to_add]
                         , eval
                         , envir = sys.parents()[which(sys.parents() == fr)[1] - 1])
                explicit_args <- c(explicit_args, args_to_add)
            }
        }
        ## stop searching frames stack at search_up_to_call call
        if (parent_call[1] %in% search_up_to_call) break()
    }
    ## merge default and explicit args
    args_updated <- default_args
    if (length(explicit_args) != 0) {
        args_updated <- 
            c(explicit_args
            , default_args[!(names(default_args) %in% names(explicit_args))])
    }
    ## just return argument if it is single argument
    if(return_unlisted_if_single_arg && length(args_updated) == 1) {
        args_updated <- args_updated[[1]]
    }
    return (args_updated)
}
## --------<<  get_dots (get from external package):1 ends here


