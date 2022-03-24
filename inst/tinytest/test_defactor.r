## -------->>  [[file:../../harmonizer.src.org::*defactor][defactor:2]]
defactor_vector <- nstandr:::defactor_vector



expect_equal(
{
    set.seed(124)
    factor(sample(c("a", "b", "b"), 20, replace = TRUE)) |> defactor_vector()
}
, c("a", "b", "b", "b", "a", "b", "a", "b", "a", "b", "b", "b", 
"b", "b", "b", "a", "b", "b", "b", "a")
)



expect_equal(
    {
        set.seed(124)
        data.frame(num = factor(sample(runif(5), 20, replace = TRUE))
                      , let = factor(sample(c("a", "b", "b"), 20, replace = TRUE))) |>
            defactor()
        }
, structure(list(num = c("0.222722708247602", "0.408794660819694", 
"0.0830154397990555", "0.515284994151443", "0.39688234962523", 
"0.39688234962523", "0.408794660819694", "0.0830154397990555", 
"0.408794660819694", "0.408794660819694", "0.39688234962523", 
"0.222722708247602", "0.408794660819694", "0.515284994151443", 
"0.515284994151443", "0.39688234962523", "0.39688234962523", 
"0.39688234962523", "0.0830154397990555", "0.515284994151443"
), let = c("b", "b", "b", "a", "b", "b", "b", "a", "b", "a", 
"a", "b", "b", "a", "b", "b", "b", "b", "a", "a")), row.names = c(NA, 
-20L), class = c("data.table", "data.frame")))





expect_equal(list(c(1,2,3), 4,5,6,7) |> 
             defactor(conv2dt = "all.but.atomic")
           , structure(list(V1 = list(c(1, 2, 3), 4, 5, 6, 7)), row.names = c(NA, 
                                                                              -5L), class = c("data.table", "data.frame")))



expect_equal(list(c(1,2,3), 4,5,6,7) |> 
             defactor(conv2dt = "only.table")
           , list(c(1, 2, 3), 4, 5, 6, 7))



expect_equal(list(c(1,2,3), 4,5,6,7) |> 
             defactor(conv2dt = "all")
           , structure(list(V1 = list(c(1, 2, 3), 4, 5, 6, 7)), row.names = c(NA, 
                                                                              -5L), class = c("data.table", "data.frame")))



expect_equal(c(c(1,2,3), 4,5,6,7) |> 
             defactor(conv2dt = "only.table")
           , c(1, 2, 3, 4, 5, 6, 7))



expect_equal(c(c(1,2,3), 4,5,6,7) |> 
             defactor(conv2dt = "all")
           , structure(list(V1 = c(1, 2, 3, 4, 5, 6, 7)), row.names = c(NA, 
                                                                        -7L), class = c("data.table", "data.frame")))
## --------<<  defactor:2 ends here


