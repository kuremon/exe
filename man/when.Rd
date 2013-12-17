\name{is.condition}
\alias{.in}
\alias{.is}
\alias{.within}
\alias{is.condition}
\alias{pair.of}
\alias{symmetric.pair.of}
\alias{when}
\title{when}
\usage{
  is.condition(x)

  when(expr, operator = "==", values)

  .is(expr, values)

  .in(expr, values)

  .within(expr, values)

  pair.of(expr, values, operator = "\%in\%")

  symmetric.pair.of(expr, values, operator = "\%in\%")
}
\description{
  when
}
\examples{
\donttest{
when(as.numeric(Year),"\%within\%",list(c(1998,2001),c(2002,2003)))
}
\donttest{
.is(Site,c("site1","site2"))
.is(Year,2001:2004)
}
\donttest{
.is(Site,c("site1","site2"))
.is(Year,2001:2004)
}
}

