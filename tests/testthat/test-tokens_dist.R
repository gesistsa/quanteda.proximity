test_that("defensive", {
    expect_error(tokens_proximity("a", "a"), "x is not a")
})

test_that(".resolve_keywords", {
    expect_equal(.resolve_keywords(c("abc", "def"), c("abcd", "defa"), valuetype = "fixed"), c("abc", "def"))
    expect_equal(.resolve_keywords(c("abc*", "def*"), c("abcd", "defa"), valuetype = "glob"), c("abcd", "defa"))
    expect_equal(.resolve_keywords(c("a"), c("abcd", "defa"), valuetype = "regex"), c("abcd", "defa"))
})
