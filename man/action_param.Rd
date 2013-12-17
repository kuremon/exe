\name{.action}
\alias{.action}
\alias{.param}
\alias{is.action}
\alias{is.param}
\title{Create an action or parameters table}
\usage{
  .action(values)

  .param(...)

  is.action(x)

  is.param(x)
}
\value{
  A data frame
}
\description{
  Create an action or parameters table
}
\examples{
\donttest{
A1=.action(c("do.this","do.that"))
A11=.action("do.this")
A2=.action(list(max,min))
A21=.action(max)
}
\donttest{
param=.param(scale=list(scale_alpha_continuous(),scale_alpha_manual()),color=c("red","blue"))
}
}

