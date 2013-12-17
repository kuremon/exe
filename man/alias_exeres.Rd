\name{alias_exeres}
\alias{alias_exeres}
\title{Alias for exeres}
\description{
  As the result of an exe called often include a naming
  function that computes name with non valid R object like
  / or - or names starting with a number it is impossible
  to access autocomplete. To bypass this restriction, exe
  assigns a class to its result \code{c("exeres","list")}.
  Structurally an exeres object is absolutely equivalent to
  its underlying list. However when an element is looked
  for with handle \code{$}, the aliases are displayed
  instead of the stored name. The stored names can still be
  retrieved with function \code{\link{real.names}}
}
\keyword{alias}
\keyword{exeres}

