## -------->>  [[file:../harmonizer.src.org::*inset_formals][inset_formals:1]]
inset_formals <- function(formals_args, new_args) {
    checkmate::assert_subset(names(new_args)
                           , choices = names(formals_args)
                           , empty.ok = FALSE)
    for (i in 1:length(new_args)) {
        formals_args[names(new_args)[i]] <- new_args[i]
    }
    return(formals_args)
}


make_alias <- function(fun, ...) {
    .fun <- fun
    ## remove 1st and 2nd elemnts ('call' name and 'fun' arg)
    args <- as.list(sys.call())[-(1:2)]
    checkmate::assert_subset(names(args)
                           , choices = names(formals(.fun))
                           , empty.ok = FALSE)
    formals(.fun)[names(args)] <- args
    return(.fun)
}
## --------<<  inset_formals:1 ends here


