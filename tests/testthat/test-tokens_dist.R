test_that("defensive", {
    expect_error(tokens_proximity("a", "a"), "x is not a")
})

test_that("edge cases", {
    expect_error("" %>% tokens() %>% tokens_proximity("") %>% convert(), NA)
})

test_that("resolve_keywords", {
    expect_equal(resolve_keywords(c("abc", "def"), c("abcd", "defa"), valuetype = "fixed"), c("abc", "def"))
    expect_equal(resolve_keywords(c("abc*", "def*"), c("abcd", "defa"), valuetype = "glob"), c("abcd", "defa"))
    expect_equal(resolve_keywords(c("a"), c("abcd", "defa"), valuetype = "regex"), c("abcd", "defa"))
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

test_that("convert", {
    suppressPackageStartupMessages(library(quanteda))
    "this is my life" %>% tokens() %>% tokens_proximity("my") %>% convert() -> res
    expect_true(is.data.frame(res))
})

test_that("convert no strange rownames, #39", {
    suppressPackageStartupMessages(library(quanteda))
    "this is my life" %>% tokens() %>% tokens_proximity("my") %>% convert() -> res
    expect_true(is.data.frame(res))
    expect_equal(rownames(res), c("1", "2", "3", "4")) ## default rownames
})

test_that("Changing keywords", {
    suppressPackageStartupMessages(library(quanteda))
    "this is my life" %>% tokens() %>% tokens_proximity("my") -> res
    expect_error(res2 <- tokens_proximity(res, "life"), NA)
    expect_equal(meta(res2, "keywords"), "life")
})

test_that("token_proximity() only emit token_proximity #35", {
    suppressPackageStartupMessages(library(quanteda))
    "this is my life" %>% tokens() %>% tokens_proximity("my") -> res
    expect_equal(class(res), "tokens_with_proximity") # no tokens
    expect_error(tokens_select(res, "life"))
    expect_error(tokens_select(as.tokens(res), "life"), NA)
})

test_that("tolower", {
    suppressPackageStartupMessages(library(quanteda))
    "this is my MIT life" %>% tokens() %>% tokens_proximity("my") -> res
    expect_false("MIT" %in% attr(res, "types"))
    "this is my MIT life" %>% tokens() %>% tokens_proximity("my", tolower = FALSE) -> res
    expect_true("MIT" %in% attr(res, "types"))
    "this is my MIT life" %>% tokens() %>% tokens_proximity("my", tolower = TRUE, keep_acronyms = TRUE) -> res
    expect_true("MIT" %in% attr(res, "types"))
    expect_true("tolower" %in% names(meta(res)))
    expect_true("keep_acronyms" %in% names(meta(res)))    
})
