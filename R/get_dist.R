.cal_dist <- function(y, poss) {
    return(abs(y - poss))
}

.get_min <- function(pos, x) {
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

get_dist <- function(x, targets, get_min = TRUE) {
    targets_poss <- which(attr(x, "types") %in% targets)
    purrr::map(x, .get_dist, targets_poss = targets_poss, get_min = get_min)
}

#' @export
tokens_dist <- function(x, targets, get_min = TRUE) {
    toks <- x
    dist <- get_dist(x = toks, targets = targets, get_min = get_min)
    quanteda::docvars(toks)$dist <- I(dist)
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
    cat("With distance vector(s).\n")
    cat("targets: ", quanteda::meta(x, field = "targets"), "\n")
}

#' @importFrom quanteda dfm
#' @method dfm tokens_with_dist
#' @export
dfm.tokens_with_dist <- function(x, remove_docvars_dist = TRUE,
                                 weight_function = function(x) {1 / x}, ...) {
    vec <- c() ## value (x) in the sparseMatrix
    i_pos <- c()
    j_pos <- c()
    feat_name <- attr(x, "types")
    x_attrs <- attributes(x)
    x_docvars <- docvars(x)
    for (i in seq_along(x)) {
        cur_dist <- quanteda::docvars(x, "dist")[[i]]
        cur_feat <- match(x[[i]], feat_name)
        unique_feat <- unique(cur_feat)
        total_vec <- rep(0, length(unique_feat))
        for(j in seq_along(unique_feat)) {
            cur_feat_j <- unique_feat[j]
            cur_vec <- cur_dist[cur_feat == cur_feat_j]
            cur_vec <- weight_function(cur_vec)
            total_vec[j] <- sum(cur_vec)
        }
        vec <- c(vec, total_vec)
        i_pos <- c(i_pos, rep(i, length(total_vec)))
        j_pos <- c(j_pos, unique_feat)
    }
    output <- quanteda::as.dfm(Matrix::sparseMatrix(i = i_pos, j = j_pos,
                                                    x = vec,
                                                    dimnames = list(quanteda::docnames(x), feat_name)))
    attributes(output)[["meta"]] <- x_attrs[["meta"]]
    if (remove_docvars_dist) {
        x_docvars$dist <- NULL
    }
    docvars(output) <- x_docvars
    return(output)
}
