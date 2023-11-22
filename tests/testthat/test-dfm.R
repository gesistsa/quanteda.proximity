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

test_that("Padding #46", {
    suppressPackageStartupMessages(library(quanteda))
    toks <- tokens(c("a b c", "A B C D")) %>% tokens_remove("b", padding = TRUE)
    expect_error(toks %>% tokens_proximity("a") %>% dfm(), NA)
})

test_that("remove_padding", {
    suppressPackageStartupMessages(library(quanteda))
    toks <- tokens(c("a b c", "A B C D")) %>% tokens_remove("b", padding = TRUE)
    output <- toks %>% tokens_proximity("a") %>% dfm()
    expect_true("" %in% colnames(output))
    output <- toks %>% tokens_proximity("a") %>% dfm(remove_padding = TRUE)
    expect_false("" %in% colnames(output))
})

test_that("force get_min #48", {
    suppressPackageStartupMessages(library(quanteda))
    toks <- tokens(c("a a b c", "A B C D"))
    toks %>% tokens_proximity("a", get_min = FALSE) -> temp2
    expect_error(dfm(temp2), NA)
    expect_message(dfm(temp2, verbose = TRUE), "Only the minimum proximity is used")
})
