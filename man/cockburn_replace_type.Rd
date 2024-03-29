% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cockburn.r
\name{cockburn_replace_type}
\alias{cockburn_replace_type}
\title{Cleanup Entity Type}
\usage{
cockburn_replace_type(x, ...)
}
\arguments{
\item{x}{vector or table}

\item{...}{
  Arguments passed on to \code{\link[=replace_patterns]{replace_patterns}}
  \describe{
    \item{\code{patterns}}{Accepts both vector or table. If patterns is a
table can also include replacements column.}
    \item{\code{patterns_col}}{If patterns is not a vector which column to
use. Default is 1.}
    \item{\code{patterns_mode}}{Mode of matching. Could be one of c("all",
"first", "last"). The default is "all" (it is 2x faster than
"first" and "last" because of handy stri_replace_all_*
functions). Also possible to pass a vector (same length as
patterns)}
    \item{\code{patterns_mode_col}}{Column in patterns table with the mode of
matching}
    \item{\code{patterns_type}}{Type of pattern for matching. Default is
"fixed" (calling
code{\link[stringi]{stri_replace_all_fixed}}). Other options
are:}
    \item{\code{patterns_type_col}}{Column with the type of pattern in case
when patterns should have different types}
    \item{\code{patterns_replacements_col}}{If patterns is not a vector and
includes replacements which column to use for
replacements. Default is 2.}
    \item{\code{replacements}}{If patterns does not have column with
replacements provide it here.}
  }}
}
\value{
standardized names table
}
\description{
Cleanup Entity Type
}
