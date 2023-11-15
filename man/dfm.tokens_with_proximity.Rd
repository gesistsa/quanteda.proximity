% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_dist.R
\name{dfm.tokens_with_proximity}
\alias{dfm.tokens_with_proximity}
\title{Create a document-feature matrix}
\usage{
\method{dfm}{tokens_with_proximity}(
  x,
  remove_docvars_proximity = TRUE,
  weight_function = function(x) {
     1/x
 },
  ...
)
}
\arguments{
\item{x}{output of \code{\link[=tokens_proximity]{tokens_proximity()}}}

\item{remove_docvars_proximity}{boolean, remove the "proximity" document variable}

\item{weight_function}{a weight function, default to invert distance}
}
\description{
Construct a sparse document-feature matrix from the output of \code{\link[=tokens_proximity]{tokens_proximity()}}.
}
\details{
By default, words closer to keywords are weighted higher. You might change that with another \code{weight_function}.
}
\examples{
library(quanteda)
ukimg_eu <- data_char_ukimmig2010 \%>\% tokens(remove_punct = TRUE) \%>\%
tokens_tolower() \%>\% tokens_proximity(c("eu", "europe", "european"))
ukimg_eu \%>\% dfm() \%>\% dfm_select(c("immig*", "migr*")) \%>\% rowSums() \%>\% sort()
## Words further away from keywords are weighted higher
ukimg_eu \%>\% dfm(weight_function = identity) \%>\% dfm_select(c("immig*", "migr*")) \%>\% rowSums() \%>\% sort()
ukimg_eu \%>\% dfm(weight_function = function(x) {1 / x^2}) \%>\%
dfm_select(c("immig*", "migr*")) \%>\% rowSums() \%>\% sort()
}