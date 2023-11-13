test_that("defensive", {
    expect_error(tokens_dist("a", "a"))
})

test_that(".resolve_targets", {
    expect_equal(.resolve_targets(c("abc", "def"), c("abcd", "defa"), valuetype = "fixed"), c("abc", "def"))
    expect_equal(.resolve_targets(c("abc*", "def*"), c("abcd", "defa"), valuetype = "glob"), c("abcd", "defa"))
    expect_equal(.resolve_targets(c("a"), c("abcd", "defa"), valuetype = "regex"), c("abcd", "defa"))
})
