% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_dist.R
\name{dfm.tokens_with_proximity}
\alias{dfm.tokens_with_proximity}
\title{Create a document-feature matrix}
\usage{
\method{dfm}{tokens_with_proximity}(
  x,
  tolower = TRUE,
  remove_padding = FALSE,
  verbose = quanteda::quanteda_options("verbose"),
  remove_docvars_proximity = TRUE,
  weight_function = function(x) {
     1/x
 },
  ...
)
}
\arguments{
\item{x}{output of \code{\link[=tokens_proximity]{tokens_proximity()}}}

\item{tolower}{ignored}

\item{remove_padding}{ignored}

\item{verbose}{ignored}

\item{remove_docvars_proximity}{boolean, remove the "proximity" document variable}

\item{weight_function}{a weight function, default to invert distance}

\item{...}{not used.}
}
\description{
Construct a sparse document-feature matrix from the output of \code{\link[=tokens_proximity]{tokens_proximity()}}.
}
\details{
By default, words closer to keywords are weighted higher. You might change that with another \code{weight_function}. Please also note that \code{tolower} and \code{remove_padding} have no effect. It is because changing tokens at this point would need to recalculate the proximity vectors. Please do all the text manipulation before running \code{\link[=tokens_proximity]{tokens_proximity()}}.
}
\examples{
library(quanteda)
tok1 <- data_char_ukimmig2010 \%>\%
    tokens(remove_punct = TRUE) \%>\%
    tokens_tolower() \%>\%
    tokens_proximity(c("eu", "europe", "european"))
tok1 \%>\%
    dfm() \%>\%
    dfm_select(c("immig*", "migr*")) \%>\%
    rowSums() \%>\%
    sort()
## Words further away from keywords are weighted higher
tok1 \%>\%
    dfm(weight_function = identity) \%>\%
    dfm_select(c("immig*", "migr*")) \%>\%
    rowSums() \%>\%
    sort()
tok1 \%>\%
    dfm(weight_function = function(x) {
        1 / x^2
    }) \%>\%
    dfm_select(c("immig*", "migr*")) \%>\%
    rowSums() \%>\%
    sort()
}