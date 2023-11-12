test_that("docvars retention", {
    suppressPackageStartupMessages(library(quanteda))
    test <- c("hello world!")
    corpus(test, docvars = data.frame(dummy = TRUE)) -> test_corpus
    meta(test_corpus, "what") <- "test"
    expect_equal(test_corpus %>% tokens() %>% dfm() %>% docvars("dummy"), TRUE)
    expect_equal(test_corpus %>% tokens() %>% tokens_dist(targets = "world") %>% dfm() %>% docvars("dummy"), TRUE)
    ## remove_docvars_dist
    docvars_cols <- test_corpus %>% tokens() %>% tokens_dist(targets = "world") %>% dfm(remove_docvars_dist = FALSE) %>% docvars() %>% colnames()
    expect_true("dist" %in% docvars_cols)
    docvars_cols <- test_corpus %>% tokens() %>% tokens_dist(targets = "world") %>% dfm(remove_docvars_dist = TRUE) %>% docvars() %>% colnames()
    expect_false("dist" %in% docvars_cols)
})

test_that("meta retention", {
    suppressPackageStartupMessages(library(quanteda))
    test <- c("hello world!")
    corpus(test, docvars = data.frame(dummy = TRUE)) -> test_corpus
    meta(test_corpus, "what") <- "test"
    expect_equal(test_corpus %>% tokens() %>% dfm() %>% meta("what"), "test")
    expect_equal(test_corpus %>% tokens() %>% tokens_dist(targets = "world") %>% dfm() %>% meta("what"), "test")
})
