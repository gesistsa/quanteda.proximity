test_that("defensive", {
    expect_error(tokens_proximity("a", "a"), "x is not a")
})

test_that(".resolve_keywords", {
    expect_equal(.resolve_keywords(c("abc", "def"), c("abcd", "defa"), valuetype = "fixed"), c("abc", "def"))
    expect_equal(.resolve_keywords(c("abc*", "def*"), c("abcd", "defa"), valuetype = "glob"), c("abcd", "defa"))
    expect_equal(.resolve_keywords(c("a"), c("abcd", "defa"), valuetype = "regex"), c("abcd", "defa"))
})

test_that("count_from", {
    suppressPackageStartupMessages(library(quanteda))
    "this is my life" %>% tokens() %>% tokens_proximity("my") %>% docvars("proximity") -> res
    expect_equal(res$text1, c(3, 2, 1, 2))
    "this is my life" %>% tokens() %>% tokens_proximity("my", count_from = 0) %>% docvars("proximity") -> res
    expect_equal(res$text1, c(2, 1, 0, 1))
    ## crazy sh*t
    "this is my life" %>% tokens() %>% tokens_proximity("my", count_from = -1) %>% docvars("proximity") -> res
    expect_equal(res$text1, c(1, 0, -1, 0))
})
