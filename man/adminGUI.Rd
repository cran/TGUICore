\name{adminGUI}
\alias{adminGUI}
\title{adminGUI}
\description{Graphical trainer interface for interactive and probably statistical courses}
\usage{adminGUI(pwd="defaultPW", evaluierung = FALSE, enableAll=TRUE) }
\arguments{
	\item{pwd}{admin-GUI may be started with password only}
  	\item{evaluierung}{should GUI be started in evaluation mode, default=FALSE}
  	\item{enableAll}{should GUI be started with all available options, default=TRUE}
}
\details{The menu entries of the GUI are defined by a csv file in the directory etc in the installation
directory of the TGUICore package.}
\value{Graphical Admin Interface}
\author{Matthias Templ, Gerlinde Dinges, Alexander Kowarik, Bernhard Meindl}
\examples{
	\dontrun{
		adminGUI()
	}	
}
\keyword{methods}