\name{OpenWindow}
\alias{OpenWindow}
\title{OpenWindow}

\description{Macro to open a window.}
\usage{OpenWindow(window=tt, frame=MainFrame, fullscreen=TRUE, title="", image=NULL, image_position="top")}

\arguments{
	\item{window}{the tk-toplevel() used.}
	\item{frame}{the name of the frame that is created.}
	\item{fullscreen}{should the window be returned fullscreen, default=TRUE.}
	\item{title}{the title of the window that is returned.}
	\item{image}{should an image be drawn in the window? default=NULL.}
	\item{image_position}{the position of the image, if an image path was specified. Default position is to draw the image on top of the window.}
}
\details{

}
\author{Alexander Kowarik, Bernhard Meindl}
\keyword{methods}