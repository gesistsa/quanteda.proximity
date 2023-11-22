test_that("normal", {
    suppressPackageStartupMessages(library(quanteda))
    testdata <-
        c("Turkish President Tayyip Erdogan, in his strongest comments yet on the Gaza conflict, said on Wednesday the Palestinian militant group Hamas was not a terrorist organisation but a liberation group fighting to protect Palestinian lands.")
    res <- testdata %>% tokens() %>% tokens_proximity(pattern = "turkish")
    res %>% dfm() -> output
    expect_equal(as.numeric(output[1,"in"]), 0.166666, tolerance = 0.0001)
})

test_that("weight function", {
    suppressPackageStartupMessages(library(quanteda))
    testdata <-
        c("Turkish President Tayyip Erdogan, in his strongest comments yet on the Gaza conflict, said on Wednesday the Palestinian militant group Hamas was not a terrorist organisation but a liberation group fighting to protect Palestinian lands.")
    res <- testdata %>% tokens() %>% tokens_proximity(pattern = "turkish")
    res %>% dfm(weight_function = identity) -> output2
    expect_equal(as.numeric(output2[1,","]), 20, tolerance = 0.0001)
})

test_that("tolower", {
    suppressPackageStartupMessages(library(quanteda))
    testdata <-
        c("Turkish President Tayyip Erdogan, in his strongest comments yet on the Gaza conflict, said on Wednesday the Palestinian militant group Hamas was not a terrorist organisation but a liberation group fighting to protect Palestinian lands.")
    res <- testdata %>% tokens() %>% tokens_proximity(pattern = "turkish", tolower = FALSE)
    res %>% dfm(tolower = TRUE) -> output
    expect_true("turkish" %in% colnames(output))
    res %>% dfm(tolower = FALSE) -> output
    expect_false("turkish" %in% colnames(output))
    res <- testdata %>% tokens() %>% tokens_proximity(pattern = phrase("Tayyip Erdogan"), tolower = FALSE)
    res %>% dfm(tolower = TRUE) -> output
    expect_true("turkish" %in% colnames(output))
})
