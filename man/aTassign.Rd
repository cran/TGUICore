\name{aTassign}
\alias{aTassign}
\alias{aTget}
\alias{aTexists}
\alias{aTrm}
\title{aTassign}
\description{assigns an object to namespace adminTGUIenv. aTget reads object from this namespace while aTexists checks for existence.}
\usage{aTassign(x,value)}
\usage{aTget(x)}
\usage{aTrm(x)}
\usage{aTexists(x)}
\arguments{
	\item{x}{the name of the object to be created/loaded/checked/removed in the given namespace.}
	\item{value}{the value/object to be assigned.}
}
\details{internal function only}
\author{Alexander Kowarik, Bernhard Meindl}
\keyword{methods}