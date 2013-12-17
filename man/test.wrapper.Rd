\name{test.wrapper}
\alias{test.wrapper}
\title{Create wrapper for test}
\usage{
  test.wrapper(test, ..., what)
}
\arguments{
  \item{test}{the name of the test to wrap}

  \item{...}{additional arguments of \code{test}}

  \item{what}{the output of the test to keep}
}
\value{
  A function that tests with \code{test(...)} and keeps
  only parameters \code{what} of the output and put them
  into a data frame.
}
\description{
  Create wrapper for test
}
\examples{
\donttest{
# Let's say for each we test if there is a difference in miles per gallon consumption between automatic and manual.
test_mpg_per_am=test.wrapper(test="t.test",formula=mpg~am,what=c("p.value","statistic"))
test_mpg_per_am(mtcars)
test_mpg_per_am(subset2(mtcars,.is(vs,1)))
}
}

