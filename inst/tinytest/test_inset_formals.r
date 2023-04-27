## -------->>  [[file:../../nstandr.src.org::*make_alias][make_alias:5]]
inset_formals <- nstandr:::inset_formals

expect_equal(inset_formals(formals(mean), alist(x = something_unevaluated))
           , alist(x = something_unevaluated, ... = ))
## --------<<  make_alias:5 ends here


