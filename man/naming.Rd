\name{create_naming}
\alias{create_naming}
\title{Create naming layout}
\usage{
  create_naming(..., sep.internal = "-")
}
\description{
  Create naming layout
}
\examples{
\donttest{
x=data.frame(.is(Site,"Paris"),.is(Year,2004),.in(Season,list(c("summer","winter"))),.is(Person,list(NULL)))
naming.folder=create_naming("Site","Season")
naming.folder(x)
naming.underscore=create_naming("Site","Season",sep.global="_")
naming.underscore(x)
}
}

