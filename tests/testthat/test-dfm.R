test_that("normal", {
    suppressPackageStartupMessages(library(quanteda))
    testdata <-
        c("Turkish President Tayyip Erdogan, in his strongest comments yet on the Gaza conflict, said on Wednesday the Palestinian militant group Hamas was not a terrorist organisation but a liberation group fighting to protect Palestinian lands.")
    res <- testdata %>% tokens() %>% tokens_tolower() %>% tokens_dist(targets = "turkish")
    res %>% dfm() -> output
    expect_equal(as.numeric(output[1,"in"]), 0.166666, tolerance = 0.0001)
})
