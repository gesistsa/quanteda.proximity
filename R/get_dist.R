#' @useDynLib quanteda.proximity row_mins_
row_mins_c <- function(mat) {
    .Call("row_mins_", mat, as.integer(nrow(mat)), as.integer(ncol(mat)))
}

.cal_dist <- function(y, poss) {
    return(abs(y - poss))
}

.get_proximity <- function(tokenized_text, keywords_poss, get_min = TRUE, count_from = 1) {
    target_idx <- which(tokenized_text %in% keywords_poss)
    poss <- seq_along(tokenized_text)
    if (length(target_idx) == 0) {
        return(rep(length(poss) + count_from, length(poss)))
    }
    res <- sapply(target_idx, .cal_dist, poss = poss)
    if (get_min) {
        return(row_mins_c(res) + count_from)
    }
    return(res)
}

get_proximity <- function(x, keywords, get_min = TRUE, count_from = 1) {
    keywords_poss <- which(attr(x, "types") %in% keywords)
    purrr::map(x, .get_proximity, keywords_poss = keywords_poss, get_min = get_min, count_from = count_from)
}

.resolve_keywords <- function(keywords, features, valuetype) {
    if (valuetype == "fixed") {
        return(keywords)
    }
    if (valuetype == "glob") {
        regex <- paste(utils::glob2rx(keywords), collapse = "|")
    }
    if (valuetype == "regex") {
        regex <- paste(keywords, collapse = "|")
    }
    grep(regex, features, value = TRUE)
}

#' Extract Distance Information
#'
#' This function extracts distance information from a [quanteda::tokens()] object.
#' @param x a `tokens` object
#' @param keywords a character vector of anchor words
#' @param get_min logical, whether to return only the minimum distance or raw distance information; it is more relevant when `keywords` have more than one word. See details.
#' @param valuetype See [quanteda::valuetype]
#' @param count_from numeric, how proximity is counted from when `get_min` is `TRUE`. The keyword is assigned with this proximity. Default to 1 (not zero) to prevent division by 0 with the default behavior of [dfm.tokens_with_proximity()].
#' @details Proximity is measured by the number of tokens away from the keyword. Given a tokenized sentence: \["I", "eat", "this", "apple"\] and suppose "eat" is the target. The vector of minimum proximity for each word from "eat" is \[2, 1, 2, 3\], if `count_from` is 1. In another case: \["I", "wash", "and", "eat", "this", "apple"\] and \["wash", "eat"\] are the keywords. The minimal distance vector is \[2, 1, 2, 1, 2, 3\]. If `get_min` is `FALSE`, the output is a list of two vectors. For "wash", the distance vector is \[1, 0, 1, 2, 3\]. For "eat", \[3, 2, 1, 0, 1, 2\].
#' It is recommended to conduct all text maniputation tasks with all `tokens_*()` functions before calling this function.
#' @return a `tokens_with_proximity` object. It is a derivative of [quanteda::tokens()], i.e. all `token_*` functions still work. A `tokens_with_proximity` has a modified [print()] method. Also, additional data slots are included
#' * a document variation `dist`
#' * a metadata slot `keywords`
#' * a metadata slot `get_min`
#' @examples
#' library(quanteda)
#' ukimg_eu <- data_char_ukimmig2010 %>%
#'     tokens(remove_punct = TRUE) %>%
#'     tokens_tolower() %>%
#'     tokens_proximity(c("eu", "euro*"))
#' ukimg_eu %>%
#'     dfm() %>%
#'     dfm_select(c("immig*", "migr*")) %>%
#'     rowSums() %>%
#'     sort()
#' ## compare with
#' data_char_ukimmig2010 %>%
#'     tokens(remove_punct = TRUE) %>%
#'     tokens_tolower() %>%
#'     dfm() %>%
#'     dfm_select(c("immig*", "migr*")) %>%
#'     rowSums() %>%
#'     sort()
#' ## rerun to select other keywords
#' ukimg_eu %>% tokens_proximity("britain")
#' @seealso [dfm.tokens_with_proximity()] [quanteda::tokens()]
#' @export
tokens_proximity <- function(x, keywords, get_min = TRUE, valuetype = c("glob", "regex", "fixed"), count_from = 1) {
    if (!inherits(x, "tokens")) {
        stop("x is not a `tokens` object.", call. = FALSE)
    }
    valuetype <- match.arg(valuetype)
    keywords <- .resolve_keywords(keywords, attr(x, "types"), valuetype)
    toks <- x
    proximity <- get_proximity(x = toks, keywords = keywords, get_min = get_min, count_from = count_from)
    quanteda::docvars(toks)$proximity <- I(proximity)
    quanteda::meta(toks, field = "keywords") <- keywords
    quanteda::meta(toks, field = "get_min") <- get_min
    class(toks) <- c("tokens_with_proximity", "tokens")
    return(toks)
}

.convert_df <- function(tokens_obj, proximity_obj, doc_id) {
    data.frame(
        "doc_id" = rep(doc_id, length(tokens_obj)),
        "token" = tokens_obj,
        "proximity" = proximity_obj
    )
}

#' @method print tokens_with_proximity
#' @export
print.tokens_with_proximity <- function(x, ...) {
    y <- x
    class(y) <- "tokens"
    print(y, ...)
    cat("With proximity vector(s).\n")
    cat("keywords: ", quanteda::meta(x, field = "keywords"), "\n")
}

#' @method convert tokens_with_proximity
#' @export
#' @importFrom quanteda convert
convert.tokens_with_proximity <- function(x, to = c("data.frame"), ...) {
    to <- match.arg(to)
    result_list <- mapply(
        FUN = .convert_df,
        tokens_obj = as.list(x),
        proximity_obj = quanteda::docvars(x, "proximity"),
        doc_id = quanteda::docnames(x),
        SIMPLIFY = FALSE
    )
    do.call(rbind, result_list)
}

#' Create a document-feature matrix
#'
#' Construct a sparse document-feature matrix from the output of [tokens_proximity()].
#' @param x output of [tokens_proximity()]
#' @param tolower ignored
#' @param remove_padding ignored
#' @param remove_docvars_proximity boolean, remove the "proximity" document variable
#' @param verbose ignored
#' @param weight_function a weight function, default to invert distance
#' @param ... not used.
#' @importFrom quanteda dfm
#' @details By default, words closer to keywords are weighted higher. You might change that with another `weight_function`. Please also note that `tolower` and `remove_padding` have no effect. It is because changing tokens at this point would need to recalculate the proximity vectors. Please do all the text manipulation before running [tokens_proximity()].
#' @examples
#' library(quanteda)
#' ukimg_eu <- data_char_ukimmig2010 %>%
#'     tokens(remove_punct = TRUE) %>%
#'     tokens_tolower() %>%
#'     tokens_proximity(c("eu", "europe", "european"))
#' ukimg_eu %>%
#'     dfm() %>%
#'     dfm_select(c("immig*", "migr*")) %>%
#'     rowSums() %>%
#'     sort()
#' ## Words further away from keywords are weighted higher
#' ukimg_eu %>%
#'     dfm(weight_function = identity) %>%
#'     dfm_select(c("immig*", "migr*")) %>%
#'     rowSums() %>%
#'     sort()
#' ukimg_eu %>%
#'     dfm(weight_function = function(x) {
#'         1 / x^2
#'     }) %>%
#'     dfm_select(c("immig*", "migr*")) %>%
#'     rowSums() %>%
#'     sort()
#' @method dfm tokens_with_proximity
#' @export
dfm.tokens_with_proximity <- function(x, tolower = TRUE, remove_padding = FALSE,
                                      verbose = quanteda::quanteda_options("verbose"), remove_docvars_proximity = TRUE,
                                      weight_function = function(x) {
                                          1 / x
                                      }, ...) {
    x_attrs <- attributes(x)
    x_docvars <- quanteda::docvars(x)
    type <- quanteda::types(x)
    temp <- unclass(x)
    index <- unlist(temp, use.names = FALSE)
    val <- weight_function(unlist(quanteda::docvars(x, "proximity"), use.names = FALSE))
    temp <- Matrix::sparseMatrix(
        j = index,
        p = cumsum(c(1L, lengths(x))) - 1L,
        x = val,
        dims = c(
            length(x),
            length(type)
        ),
        dimnames = list(quanteda::docnames(x), type)
    )
    output <- quanteda::as.dfm(temp)
    attributes(output)[["meta"]] <- x_attrs[["meta"]]
    if (remove_docvars_proximity) {
        x_docvars$proximity <- NULL
    }
    quanteda::docvars(output) <- x_docvars
    return(output)
}
