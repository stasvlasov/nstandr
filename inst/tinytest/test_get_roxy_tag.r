## -------->>  [[file:../../nstandr.src.org::*make_alias][make_alias:2]]
make_roxy_tags <- nstandr:::make_roxy_tags
add_attr <- nstandr:::add_attr

a <- function() {}

add_attr(a
       , .title = "hello"
       , .example = "1 + 2 -> 3"
       , .description = "Lorem ipsum dolor sit amet, consectetur adipisicing elit. Proin nibh augue, suscipit a, scelerisque sed, lacinia in, mi. Cras vel lorem. Etiam pellentesque aliquet tellus.")

expect_equal(
    make_roxy_tags(a)
  , c("@title hello", "@description", "Lorem ipsum dolor sit amet, consectetur adipisicing elit. Proin nibh", 
      "augue, suscipit a, scelerisque sed, lacinia in, mi. Cras vel lorem.", 
      "Etiam pellentesque aliquet tellus.", "", "A simple illustration of what this procedure does:", 
      "", "    1 + 2 -> 3"))
## --------<<  make_alias:2 ends here


