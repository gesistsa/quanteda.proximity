#' @useDynLib quanteda.proximity row_mins_
row_mins_c <- function(mat) {
    .Call("row_mins_", mat, as.integer(nrow(mat)), as.integer(ncol(mat)))
}

cal_dist <- function(y, poss) {
    return(abs(y - poss))
}

cal_proximity <- function(tokenized_text, keywords_poss, get_min = TRUE, count_from = 1) {
    target_idx <- which(tokenized_text %in% keywords_poss)
    poss <- seq_along(tokenized_text)
    if (length(target_idx) == 0) {
        return(rep(length(poss) + count_from, length(poss)))
    }
    res <- sapply(target_idx, cal_dist, poss = poss)
    if (get_min) {
        return(row_mins_c(res) + count_from)
    }
    return(res)
}

get_proximity <- function(x, keywords, get_min = TRUE, count_from = 1) {
    keywords_poss <- which(attr(x, "types") %in% keywords)
    return(lapply(unclass(x), cal_proximity, keywords_poss = keywords_poss, get_min = get_min, count_from = count_from))
}

resolve_keywords <- function(keywords, features, valuetype) {
    if (valuetype == "fixed") {
        return(keywords)
    }
    if (valuetype == "glob") {
        regex <- paste(utils::glob2rx(keywords), collapse = "|")
    }
    if (valuetype == "regex") {
        regex <- paste(keywords, collapse = "|")
    }
    return(grep(regex, features, value = TRUE))
}

#' Extract Proximity Information
#'
#' This function extracts distance information from a [quanteda::tokens()] object.
#' @param x a `tokens` or `tokens_with_proximity` object.
#' @param pattern pattern for selecting keywords, see [quanteda::pattern] for details.
#' @param get_min logical, whether to return only the minimum distance or raw distance information; it is more relevant when `keywords` have more than one word. See details.
#' @param valuetype See [quanteda::valuetype].
#' @param count_from numeric, how proximity is counted from when `get_min` is `TRUE`. The keyword is assigned with this proximity. Default to 1 (not zero) to prevent division by 0 with the default behaviour of [dfm.tokens_with_proximity()].
#' @param tolower logical, convert all features to lowercase.
#' @param keep_acronyms logical, if `TRUE`, do not lowercase any all-uppercase words. See [quanteda::tokens_tolower()].
#' @details Proximity is measured by the number of tokens away from the keyword. Given a tokenized sentence: \["I", "eat", "this", "apple"\] and suppose "eat" is the keyword. The vector of minimum proximity for each word from "eat" is \[2, 1, 2, 3\], if `count_from` is 1. In another case: \["I", "wash", "and", "eat", "this", "apple"\] and \["wash", "eat"\] are the keywords. The minimal distance vector is \[2, 1, 2, 1, 2, 3\]. If `get_min` is `FALSE`, the output is a list of two vectors. For "wash", the distance vector is \[1, 0, 1, 2, 3\]. For "eat", \[3, 2, 1, 0, 1, 2\].
#' Please conduct all text maniputation tasks with `tokens_*()` functions before calling this function. To convert the output back to a `tokens` object, use [quanteda::as.tokens()].
#' @return a `tokens_with_proximity` object. It is similar to [quanteda::tokens()], but only [dfm.tokens_with_proximity()], [quanteda::convert()], [quanteda::docvars()], and [quanteda::meta()] methods are available. A `tokens_with_proximity` has a modified [print()] method. Also, additional data slots are included
#' * a document variation `dist`
#' * a metadata slot `keywords`
#' * a metadata slot `get_min`
#' * a metadata slot `tolower`
#' * a metadata slot `keep_acronyms`
#' @examples
#' library(quanteda)
#' tok1 <- data_char_ukimmig2010 %>%
#'     tokens(remove_punct = TRUE) %>%
#'     tokens_tolower() %>%
#'     tokens_proximity(c("eu", "euro*"))
#' tok1 %>%
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
#' tok1 %>% tokens_proximity("britain")
#' @seealso [dfm.tokens_with_proximity()] [quanteda::tokens()]
#' @export
tokens_proximity <- function(x, pattern, get_min = TRUE, valuetype = c("glob", "regex", "fixed"), count_from = 1,
                             tolower = TRUE, keep_acronyms = FALSE) {
    if (!inherits(x, "tokens") && !inherits(x, "tokens_with_proximity")) {
        stop("x is not a `tokens` or `tokens_with_proximity` object.", call. = FALSE)
    }
    if (inherits(x, "tokens_with_proximity")) {
        x <- as.tokens(x, remove_docvars_proximity = TRUE)
    }
    if (tolower) {
        x <- quanteda::tokens_tolower(x, keep_acronyms = keep_acronyms)
    }
    valuetype <- match.arg(valuetype)
    keywords <- resolve_keywords(pattern, attr(x, "types"), valuetype)
    toks <- x
    proximity <- get_proximity(x = toks, keywords = keywords, get_min = get_min, count_from = count_from)
    quanteda::docvars(toks)$proximity <- I(proximity)
    quanteda::meta(toks, field = "keywords") <- keywords
    quanteda::meta(toks, field = "get_min") <- get_min
    quanteda::meta(toks, field = "tolower") <- tolower
    quanteda::meta(toks, field = "keep_acronyms") <- keep_acronyms
    class(toks) <- c("tokens_with_proximity")
    return(toks)
}

convert_df <- function(tokens_obj, proximity_obj, doc_id) {
    return(data.frame(
        "doc_id" = rep(doc_id, length(tokens_obj)),
        "token" = tokens_obj,
        "proximity" = proximity_obj
    ))
}

#' @method print tokens_with_proximity
#' @export
print.tokens_with_proximity <- function(x, ...) {
    print(as.tokens(x), ...)
    cat("With proximity vector(s).\n")
    cat("keywords: ", quanteda::meta(x, field = "keywords"), "\n")
}

#' @importFrom quanteda as.tokens
#' @method as.tokens tokens_with_proximity
#' @export
as.tokens.tokens_with_proximity <- function(x, concatenator = "/", remove_docvars_proximity = TRUE, ...) {
    if (remove_docvars_proximity) {
        attr(x, which = "docvars")$proximity <- NULL
    }
    class(x) <- "tokens"
    return(x)
}

#' @importFrom quanteda docvars
#' @method docvars tokens_with_proximity
#' @export
docvars.tokens_with_proximity <- function(x, field = NULL) {
    quanteda::docvars(as.tokens(x, remove_docvars_proximity = FALSE), field = field)
}

#' @importFrom quanteda meta
#' @method meta tokens_with_proximity
#' @export
meta.tokens_with_proximity <- function(x, field = NULL, type = c("user", "object", "system", "all")) {
    quanteda::meta(as.tokens(x, remove_docvars_proximity = FALSE), field = field, type = type)
}

#' @method convert tokens_with_proximity
#' @export
#' @importFrom quanteda convert
convert.tokens_with_proximity <- function(x, to = c("data.frame"), ...) {
    to <- match.arg(to)
    x_docnames <- attr(x, "docvars")$docname_
    result_list <- mapply(
        FUN = convert_df,
        tokens_obj = as.list(x),
        proximity_obj = quanteda::docvars(x, "proximity"),
        doc_id = x_docnames,
        SIMPLIFY = FALSE, USE.NAMES = FALSE
    )
    return(do.call(rbind, result_list))
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
#' tok1 <- data_char_ukimmig2010 %>%
#'     tokens(remove_punct = TRUE) %>%
#'     tokens_tolower() %>%
#'     tokens_proximity(c("eu", "europe", "european"))
#' tok1 %>%
#'     dfm() %>%
#'     dfm_select(c("immig*", "migr*")) %>%
#'     rowSums() %>%
#'     sort()
#' ## Words further away from keywords are weighted higher
#' tok1 %>%
#'     dfm(weight_function = identity) %>%
#'     dfm_select(c("immig*", "migr*")) %>%
#'     rowSums() %>%
#'     sort()
#' tok1 %>%
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
    x_docnames <- attr(x, "docvars")$docname_
    type <- attr(x, "types")
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
        dimnames = list(x_docnames, type)
    )
    output <- quanteda::as.dfm(temp)
    attributes(output)[["meta"]] <- x_attrs[["meta"]]
    if (remove_docvars_proximity) {
        x_docvars$proximity <- NULL
    }
    quanteda::docvars(output) <- x_docvars
    return(output)
}
