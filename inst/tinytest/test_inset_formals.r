## -------->>  [[file:../../harmonizer.src.org::*inset_formals][inset_formals:1]]
inset_formals <- harmonizer:::inset_formals

expect_equal(inset_formals(formals(mean), alist(x = something_unevaluated))
           , alist(x = something_unevaluated, ... = ))
## --------<<  inset_formals:1 ends here


