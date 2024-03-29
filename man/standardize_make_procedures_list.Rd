% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/0_standardize_make_procedures_list.r
\name{standardize_make_procedures_list}
\alias{standardize_make_procedures_list}
\title{Makes list of procedures calls from table.}
\usage{
standardize_make_procedures_list(
  procedures_table,
  message_field = "message",
  function_call_field = "function.call",
  no_field = "no",
  remove_comments = TRUE,
  sort_by_no_field = TRUE,
  comments = c("#", "-", "")
)
}
\arguments{
\item{procedures_table}{Table to use}

\item{message_field}{name of the column with messages that will be
displayed when each call is executed}

\item{function_call_field}{name of the column where function
(standardization procedures) calls are listed.}

\item{no_field}{name of the column where the number of procedure
is specified. Also this field indicates if the row in the
table is just a comment in which case it will be removed if
\code{remove_comments} is set (which is set by default)}

\item{remove_comments}{Whether to remove comments.}

\item{sort_by_no_field}{Whether to sort the list by col named
\code{no_field}}

\item{comments}{Values (character string) in the first col that
makes entire row as commented out}
}
\value{
List of named function calls. Names are messages.
}
\description{
Table should have at least two columns - messages and fuctions
calls. Each function call should be a string of the following
format "'function.name', arg1 = val1, arg2 = val2" (same as
arguments for \code{do.call} function).
}
