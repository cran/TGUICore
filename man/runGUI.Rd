\name{runGUI}
\alias{runGUI}
\title{runGUI}
\description{Graphical student interface for interactive and probably statistical courses}
\usage{runGUI(onlyCourse=NULL)}
\arguments{
\item{onlyCourse}{should be set to a specific course name for only handling on course of contents.csv}
}
\details{
runGUI() starts the Graphical User Interface for Students. The functions parses the mandatory contents.csv file and automatically creates all the neccessary menu-entries.
Furthermore, by default runGUI() allows all exercises available to be tested. In order to lock exercises, trainers need to use the admin interface (adminGUI()).
}
\value{Graphical User Interface}
\author{Matthias Templ, Gerlinde Dinges, Alexander Kowarik, Bernhard Meindl}
\examples{
\dontrun{
	runGUI()
}	
}
\keyword{methods}