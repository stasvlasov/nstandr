## -------->>  [[file:../../harmonizer.src.org::*inset_formals][inset_formals:2]]
inset_formals <- nstandr:::inset_formals

expect_equal(inset_formals(formals(mean), alist(x = something_unevaluated))
           , alist(x = something_unevaluated, ... = ))
## --------<<  inset_formals:2 ends here


