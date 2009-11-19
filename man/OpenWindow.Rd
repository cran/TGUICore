\name{OpenWindow}
\alias{OpenWindow}
\alias{SingleChoice}
\alias{SingleChoiceAnswer}
\alias{MultipleChoice}
\alias{MultipleChoiceAnswer}
\alias{InteractivePlot}
\alias{Feedback}
\alias{FeedbackAnswer}
\alias{SingleChoiceAnswerGrid}
\alias{SingleChoiceGrid}
\title{OpenWindow}

\description{Macros that draw either complete Windows or complete Frames depending on input-Parameters.}
\usage{OpenWindow(window=tt, frame=MainFrame, fullscreen=TRUE, title="", image=NULL, image_position="top")}
\usage{Feedback(frame=MainFrame,name,question1,question2=NULL,labels,image=NULL,filename)}
\usage{FeedbackAnswer(frame=MainFrame,name,question1,question2=NULL,labels,image=NULL,filename)}
\arguments{
	\item{window}{the tk-toplevel() used.}
	\item{frame}{the name of the frame that is created.}
	\item{fullscreen}{should the window be returned fullscreen, default=TRUE.}
	\item{title}{the title of the window that is returned.}
	\item{image}{should an image be drawn in the window? default=NULL.}
	\item{image_position}{the position of the image, if an image path was specified. Default position is to draw the image on top of the window.}
}
\details{internal function only}
\author{Alexander Kowarik, Bernhard Meindl}
\keyword{methods}