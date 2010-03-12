\name{Tassign}
\alias{Tassign}
\alias{Tget}
\alias{Texists}
\alias{Trm}
\title{Tassign}
\description{assigns an object to namespace TGUIenv. Tget reads object from this namespace while Texists checks for existence.}
\usage{Tassign(x,value)}
\usage{Tget(x)}
\usage{Trm(x)}
\usage{Texists(x)}


\arguments{
	\item{x}{the name of the object to be created/loaded/checked/removed in the given namespace.}
	\item{value}{the value/object to be assigned.}
}
\details{internal function only}
\author{Alexander Kowarik, Bernhard Meindl}
\keyword{methods}