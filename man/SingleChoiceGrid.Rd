\name{SingleChoiceGrid}
\alias{SingleChoiceGrid}
\title{SingleChoiceGrid}

\description{Macros that draw either complete Windows or complete Frames depending on input-Parameters.}
\usage{SingleChoiceGrid(frame=MainFrame, name,question1,question2=NULL,labels1,labels2,
    hscale=2,vscale=1.5,hscaleAns=1,vscaleAns=1,image=NULL,plotFunction=NULL,
    note=NULL,Answer=FALSE,filename)}

\arguments{
	\item{frame}{the name of the frame that is created.}
	\item{name}{the name of the frame that is created and which includes the ExitButton}
	\item{question1}{Single question}
	\item{question2}{vector of additional lines for explaining the questions}
	\item{labels1}{Labels for the columns of the answer grid}
	\item{labels2}{Lables for the rows of the answer grid}
	\item{image}{filename of a gif image to be included}
	\item{hscale}{horizontal scale for the included plot}
	\item{vscale}{vertical scale for the included plot}
	\item{hscaleAns}{horizontal scale for the plot of the answers}
	\item{vscaleAns}{vertical scale for the plot of the answers}
	\item{plotFunction}{function which creates the plot(without parameters) for the question}
	\item{filename}{name of a file in which the answers will be saved}
	\item{note}{a string defining a note for the participant}
	\item{Answer}{TRUE for showing the evaluation view of the Feedback widget}
}
\details{for example see package TGUITeaching}
\author{Alexander Kowarik, Bernhard Meindl}
\keyword{methods}
