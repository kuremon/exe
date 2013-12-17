\name{subset2i}
\alias{subset2i}
\title{Subset interactively on several arguments}
\usage{
  subset2i(data, ...)
}
\arguments{
  \item{data}{}

  \item{...}{conditions written as in
  \code{\link{transform}}}
}
\value{
  The subset data frame where all the conditions in
  \code{...} have been combined by AND.
}
\description{
  Subset on several simultaneous conditions.
}
\examples{
\donttest{
subset2i(mtcars,cyl\%in\%c(4,6),am==1,vs==0)
subset2i(mtcars,cyl\%in\%c(4,6),am==1,Group==2)
subset2i(mtcars,mpg==2)
}
}
\seealso{
  \code{\link{subset2}}
}

