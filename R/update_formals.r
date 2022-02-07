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


make_alias <- function(fun, new_formals_alist) {
    default_formals <- formals(fun)
    checkmate::assert_subset(names(new_formals_alist)
                           , choices = names(default_formals)
                           , empty.ok = FALSE)
    for (i in 1:length(new_formals_alist)) {
        default_formals[names(new_formals_alist)[i]] <- new_formals_alist[i]
    }
    formals(fun) <- default_formals
    return(fun)
}
## --------<<  inset_formals:1 ends here


