\name{InteractivePlot}
\alias{InteractivePlot}
\title{InteractivePlot}

\description{Macros that draw either complete Windows or complete Frames depending on input-parameters.}
\usage{InteractivePlot(frame=MainFrame, name, plot.function, click.function=NULL,
	hscale=2, vscale=2,
    slider=NULL, slider_lim=NULL, slider_start=NULL, slider_res=NULL, slider_label=NULL, slider_header=NULL,
    checkbox=NULL, checkbox_start=NULL, checkbox_header=NULL, checkbox_label=NULL, checkbox_twoCols=FALSE,
    radio=NULL, radio_label=NULL, radio_header=NULL, radio_value=NULL,
    header=NULL,design="leftright")}
\arguments{
	\item{frame}{the frame in which the ExitButton should be drawn.}
	\item{name}{the name of the frame that is created and which includes the ExitButton}
	\item{plot.function}{function which creates the plot(without parameters) for a given question}
	\item{click.function}{function which should be called if interaction with a plot using mouse-clicks is possible}
	\item{hscale}{horizontal scale}
	\item{vscale}{vertical scale}
	\item{slider}{slider names}
	\item{slider_lim}{slider limits}
	\item{slider_start}{initial values for the sliders}
	\item{slider_res}{Resolution of the sliders}
	\item{slider_label}{caption of the slider}
	\item{slider_header}{title for all sliders}
	\item{checkbox}{names of the checkbox objects}
	\item{checkbox_start}{initial value of the checkbox}
	\item{checkbox_header}{title for all checkboxes}
	\item{checkbox_label}{caption of the checkbox}
	\item{checkbox_twoCols}{if TRUE the checkboxes are arranges in two columns, otherwise only in one}
	\item{radio}{names of the radiobutton objects}
	\item{radio_label}{caption of the radiobuttons}
	\item{radio_header}{title for all radiobuttons}
	\item{radio_value}{values of the radiobuttons}
    \item{header}{title for the plot}
    \item{design}{if set to leftright, the plot is on the right side and the controls are on the left side, otherwise the controls are below the plot}
 }
\details{internal function only}
\author{Alexander Kowarik, Bernhard Meindl}
\keyword{methods}