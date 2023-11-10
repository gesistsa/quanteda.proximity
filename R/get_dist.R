.cal_dist <- function(y, poss) {
    return(abs(y - poss))
}

.get_min <- function(pos, x) {
    ##print(pos)
    min(purrr::map_dbl(x, pos))
}

.get_dist <- function(tokenized_text, targets_poss, get_min = TRUE) {
    target_idx <- which(tokenized_text %in% targets_poss)
    poss <- seq_along(tokenized_text)
    if (length(target_idx) == 0) {
        return(rep(length(poss) + 1, length(poss)))
    }
    res <- lapply(target_idx, .cal_dist, poss = poss)
    if (get_min) {
        return(purrr::map_dbl(poss, .get_min, x = res) + 1)
    }
    return(res)
}

#' @export
get_dist <- function(x, targets, get_min = TRUE) {
    targets_poss <- which(attr(x, "types") %in% targets)
    purrr::map(x, .get_dist, targets_poss = targets_poss, get_min = get_min)
}

#' @export
tokens_dist <- function(x, targets, get_min = TRUE) {
    toks <- x
    quanteda::meta(toks, field = "dist") <- get_dist(x = toks, targets = targets, get_min = get_min)
    quanteda::meta(toks, field = "targets") <- targets
    quanteda::meta(toks, field = "get_min") <- get_min
    class(toks) <- c("tokens_with_dist", "tokens")
    return(toks)
}


#' @method print tokens_with_dist
#' @export
print.tokens_with_dist <- function(x, ...) {
    y <- x
    class(y) <- "tokens"
    print(y, ...)
    cat("With distance vector.\n")
    cat("targets: ", quanteda::meta(x, field = "targets"), "\n")
}
