## -------->>  [[file:../nstandr.src.org::*make_alias][make_alias:1]]
inset_formals <- function(formals_args, new_args) {
    checkmate::assert_subset(names(new_args)
                           , choices = names(formals_args)
                           , empty.ok = FALSE)
    for (i in 1:length(new_args)) {
        formals_args[names(new_args)[i]] <- new_args[i]
    }
    return(formals_args)
}



add_attr <- function(fun
                   , env = parent.frame()
                   , attr_prefix = "nstandr_procedure_"
                   , ...) {
    .fun <- substitute(fun)
    checkmate::assert_character(...names()
                              , pattern = "^\\..+"
                              , null.ok = TRUE)
    mapply(
        \(name, val) {
            name <- sub("^\\.", attr_prefix, name)
            eval(bquote(attr(.(.fun), .(name)) <- .(val)), envir = env)
        }
      , ...names()
      , list(...))
}

make_alias <- function(f, ...) {
    alias_name = deparse(substitute(f))
    ## remove 1st and 2nd elements ('call' name and 'f' arg)
    args <- as.list(sys.call())[-(1:2)]
    args_attr_p <- grepl("^\\..+", names(args))
    f_args <- args[!args_attr_p]
    checkmate::assert_subset(names(f_args)
                           , choices = names(formals(f))
                           , empty.ok = FALSE)
    ## formals resets attributes so it comes first
    formals(f)[names(f_args)] <- f_args
    eval(bquote(add_attr(f
                       , ..(args[args_attr_p])
                       , .alias = alias_name)
              , splice = TRUE))
    return(f)
}

#' title
#' tables (with reference, pp, table no. and title)
#' example
#' description (very short and just ref to pp)
#' reference (url, doi, authors, can I use noweb with papis call to ref to extract all that?)

make_roxy_tags <- function(f) {
    get_attr <- function(a) {
        attr(f, paste0("nstandr_procedure_", a))
    }
    get_roxy_tag <- function(tag, name = NULL) {
        val <- get_attr(tag)
        if(is.null(val)) {
            return(NULL)
        } else {
            tag <- if(is.null(name)) tag else name
            return(paste0("@", tag, " ",val))
        }
    }
    description <-
        c(if(!is.null(get_attr("description"))) {
              c(stringi::stri_wrap(get_attr("description")), "")
          }
        , if(!is.null(get_attr("example"))) {
              c("A simple illustration of what this procedure does:", ""
              , paste0("    ", get_attr("example")))
          })
    description <- if(!is.null(description)) {
                       c("@description", description)
                   }
    c(get_roxy_tag("title")
    , description
      ## details
      ## Put name of the alias function to @seealso
    , get_roxy_tag("alias", "seealso"))
}
## --------<<  make_alias:1 ends here


