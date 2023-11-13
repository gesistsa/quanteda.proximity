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

.resolve_targets <- function(targets, features, valuetype) {
    if (valuetype == "fixed") {
        return(targets)
    }
    if (valuetype == "glob") {
        regex <- paste(glob2rx(targets), collapse = "|")
    }
    if (valuetype == "regex") {
        regex <- paste(targets, collapse = "|")
    }
    grep(regex, features, value = TRUE)
}

#' Extract Distance Information
#'
#' This function extracts distance information from a [quanteda::tokens()] object.
#' @param x a `tokens` object
#' @param targets a character vector of anchor words
#' @param get_min logical, whether to return only the minimum distance or raw distance information; it is more relevant when `targets` have more than one word. See details.
#' @param valuetype See [quanteda::valuetype]
#' @details The distance is measured by number of tokens away from the target. Given a tokenized sentence: ["I", "eat", "this", "apple"] and suppose "eat" is the target. The vector of minimum distances for each word from "eat" is [2, 1, 2, 3]. In another case: ["I", "wash", "and", "eat", "this", "apple"] and ["wash", "eat"] are the targets. The minimal distance vector is [2, 1, 2, 1, 2, 3]. If `get_min` is `FALSE`, the output is a list of two vectors. For "wash", the distance vector is [1, 0, 1, 2, 3]. For "eat", [3, 2, 1, 0, 1, 2]. `get_min` always add 1 to the distance.
#' It is recommended to conduct all text maniputation tasks with all `tokens_*()` functions before calling this function.
#' @return a `tokens_with_dist` object. It is a derivative of [quanteda::tokens()], i.e. all `token_*` functions still work. A `tokens_with_dist` has a modified [print()] method. Also, additional data slots are included
#' * a document variation `dist`
#' * a metadata slot `targets`
#' * a metadata slot `get_min`
#' @examples
#' library(quanteda)
#' ukimg_eu <- data_char_ukimmig2010 %>% tokens(remove_punct = TRUE) %>%
#' tokens_tolower() %>% tokens_dist(c("eu", "euro*"))
#' ukimg_eu %>% dfm() %>% dfm_select(c("immig*", "migr*")) %>% rowSums() %>% sort()
#' ## compare with
#' data_char_ukimmig2010 %>% tokens(remove_punct = TRUE) %>% tokens_tolower() %>%
#' dfm %>% dfm_select(c("immig*", "migr*")) %>% rowSums() %>% sort()
#' ## rerun to select other targets
#' ukimg_eu %>% tokens_dist("britain")
#' @seealso [dfm.tokens_with_dist()] [quanteda::tokens()]
#' @export
tokens_dist <- function(x, targets, get_min = TRUE, valuetype = c("glob", "regex", "fixed")) {
    if (!inherits(x, "tokens")) {
        stop("x is not a `tokens` object.", call. = FALSE)
    }
    valuetype <- match.arg(valuetype)
    targets <- .resolve_targets(targets, attr(x, "types"), valuetype)
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


#' Create a document-feature matrix
#'
#' Construct a sparse document-feature matrix from the output of [tokens_dist()].
#' @param x output of [tokens_dist()]
#' @param remove_docvars_dist boolean, remove the "dist" document variable
#' @param weight_function a weight function, default to invert distance
#' @importFrom quanteda dfm
#' @details By default, words closer to targets are weighted higher. You might change that with another `weight_function`.
#' @examples
#' library(quanteda)
#' ukimg_eu <- data_char_ukimmig2010 %>% tokens(remove_punct = TRUE) %>%
#' tokens_tolower() %>% tokens_dist(c("eu", "europe", "european"))
#' ukimg_eu %>% dfm() %>% dfm_select(c("immig*", "migr*")) %>% rowSums() %>% sort()
#' ## Words further away from targets are weighted higher
#' ukimg_eu %>% dfm(weight_function = identity) %>% dfm_select(c("immig*", "migr*")) %>% rowSums() %>% sort()
#' ukimg_eu %>% dfm(weight_function = function(x) {1 / x^2}) %>%
#' dfm_select(c("immig*", "migr*")) %>% rowSums() %>% sort()
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
