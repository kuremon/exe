\name{subset2}
\alias{subset2}
\title{Subset non interactively on several arguments}
\usage{
  subset2(data, cond)
}
\arguments{
  \item{data}{the data frame to subset}

  \item{cond}{a condition data frame}
}
\value{
  The subset data frame where all the conditions specified
  in \code{cond} have been combined together with an AND.
  by a logical.
}
\description{
  A function to subset non interactively on several
  arguments combined by AND using condition data
  frames/list.
}
\details{
  Conditions on non existing columns of \code{data} and on
  \code{NULL} values are ignored.
}
\examples{
\donttest{
cond=cbind(.in(Petal.Width,list(c(0.2,0.3))),.is(as.numeric(as.factor(Species)),1))
subset2(iris,cond)
cond1=cbind(cond,.is(Group,"flower"))
subset2(iris,cond1)
cond2=cbind(cond,.is(Sepal.Length,list(NULL)))
subset2(iris,cond2)
}
}

