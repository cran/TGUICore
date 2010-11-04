\name{adminGUI}
\alias{adminGUI}
\title{adminGUI}
\description{Graphical trainer interface for interactive and probably statistical courses}
\usage{adminGUI(onlyCourse=NULL, pwd="defaultPW",evaluierung = FALSE, enableAll=FALSE) }
\arguments{
	\item{onlyCourse}{should be set to a specific course name for only handling on course of contents.csv}
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