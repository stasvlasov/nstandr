% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cockburn.r
\name{standardize_cockburn}
\alias{standardize_cockburn}
\title{Standardizes strings using exact procedures described in Cockburn, et al. (2009)}
\usage{
standardize_cockburn(
  x,
  cockburn_procedures = nstandr_cookburn_procedures_list,
  detect_legal_form = FALSE,
  return_x_before_common_words_removal = FALSE,
  ...
)
}
\arguments{
\item{x}{table or vector}

\item{cockburn_procedures}{list of procedures to pass to \code{standardize} function.}

\item{detect_legal_form}{Whether to detect legal forms. Default is FALSE}

\item{return_x_before_common_words_removal}{Whether to save standardized column before \code{common.words.removal} procedure. Default is FALSE}

\item{...}{
  Arguments passed on to \code{\link[=standardize]{standardize}}
  \describe{
    \item{\code{procedures}}{The procedures that basically comprise the standardization algorithm are specified as a list of either (1) names of procedures function as character strings or as (2) calls where you can provide optional arguments or (3) nested lists that allow user to group procedures. Nesting lists of procedures has an effect on standardization progress reporting and on visualizing algorithms with \code{nstandr_plot}. Technically nested lists are equivalent to plain list of procedures as it should produce same results. Names of the list elements are used for progress messages. For unnamed elements the name of procedure's function will be used for standardization progress reporting. Default is \code{nstandr:::nstandr_default_procedures_list}.}
    \item{\code{show_progress}}{Whether to report progress percentage. Default is TRUE}
    \item{\code{nrows_min_to_show_progress}}{The minimum number of rows the x should have for automatic progress estimation. If x has less rows no progress will be shown. Default is 10^5}
    \item{\code{progress_step_nrows}}{If set it will divide the x into chunk of this amount of rows. Default is NULL.}
    \item{\code{progress_step_in_percent}}{Number of percents that represent one step in progress. Value should be between 0.1 and 50. Default is 1 which means it will try to chunk the x into 100 pieces.}
    \item{\code{progress_message_use_names}}{Should we use names from \code{procedures} list to report progress. Default is TRUE.}
    \item{\code{quite}}{Suppress all messages. Default is FALSE.}
    \item{\code{save_intermediate_x_to_var}}{For debuging of standartization procedures. Saves intermediate results to this variable. If procedures finish without errors then the variable will be removed.}
  }}
}
\value{
standardized names table
}
\description{
Standardizes strings using exact procedures described in Cockburn, et al. (2009)
}
\references{
Cockburn, et al. (2009)
}
