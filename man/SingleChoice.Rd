\name{SingleChoice}
\alias{SingleChoice}
\alias{MultipleChoice}
\title{SingleChoice}

\description{Macros that draw either complete Windows or complete Frames depending on input-Parameters.}
\usage{SingleChoice(frame=MainFrame,name,question1,question2=NULL,labels,image=NULL,hscale=1.5,vscale=1,
   hscaleAns=1,vscaleAns=1,plotFunction=NULL,filename,note=NULL,Answer=FALSE,dynSol=FALSE)}
\usage{MultipleChoice(frame=MainFrame,name,question1,question2=NULL,labels,image=NULL,hscale=1.5,vscale=1,
  hscaleAns=1,vscaleAns=1,plotFunction=NULL,filename,note=NULL,Answer=FALSE,dynSol=FALSE)}
\arguments{
	\item{frame}{the name of the frame that is created.}
	\item{name}{the name of the frame that is created and which includes the ExitButton}
	\item{question1}{Single question}
	\item{question2}{vector of additional lines for explaining the questions}
	\item{labels}{vector of possible answers}
	\item{image}{filename of a gif image to be included}
	\item{hscale}{horizontal scale for the included plot}
	\item{vscale}{vertical scale for the included plot}
	\item{hscaleAns}{horizontal scale for the plot of the answers}
	\item{vscaleAns}{vertical scale for the plot of the answers}
	\item{plotFunction}{function which creates the plot(without parameters) for the question}
	\item{filename}{name of a file in which the answers will be saved}
	\item{note}{a string defining a note for the participant}
	\item{dynSol}{set to TRUE, if there is no correct solution, but a dynamical solution}
	\item{Answer}{TRUE for showing the evaluation view of the Feedback widget}
}
\details{for example see package TGUITeaching}
\author{Alexander Kowarik, Bernhard Meindl}
\keyword{methods}
