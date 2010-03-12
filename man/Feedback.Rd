\name{Feedback}
\alias{Feedback}
\title{Feedback}

\description{Macros that draw either complete Windows or complete Frames depending on input-Parameters.}
\usage{Feedback(frame=MainFrame,name,question1,question2=NULL,image=NULL,Answer=FALSE,filename)}
\arguments{
	\item{frame}{the frame in which the ExitButton should be drawn.}
	\item{name}{the name of the frame that is created and which includes the ExitButton}
	\item{question1}{the title for the Feedback frame}
	\item{question2}{vector of questions}
	\item{image}{filename of a gif image to be included}
	\item{Answer}{TRUE for showing the evaluation view of the Feedback widget}
	\item{filename}{filename to save the entered information in}
}
\details{for example see package TGUITeaching}
\author{Alexander Kowarik, Bernhard Meindl}
\keyword{methods}