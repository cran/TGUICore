\name{plotTable}
\alias{plotTable}
\title{plotTable}

\description{plotTable into tk-window.}
\usage{
plotTable(table, lwd = par("lwd"), bg = par("bg"), 
    cex=1, xjust = 0, yjust = 1, box.col = par("fg"), text.col = par("fg"), 
    display.colnames = TRUE, display.rownames = TRUE, hlines = TRUE, vlines=TRUE,mar=c(0,0,1,0), 
    title = NULL,col1="red",col2="orange",column1=NULL,column2=NULL,mwidth=0.02539063,
    cellwidth=0.08398439,cellheight=0.04251454)
}

\arguments{
	\item{table}{an R table object}
	\item{lwd}{line width}
	\item{bg}{background}
	\item{cex}{relative scaling parameter}
	\item{xjust}{adjustment, x-scale}
	\item{yjust}{adjustment, y-scale}
	\item{box.col}{foreground color}
	\item{text.col}{text color}
	\item{display.colnames}{display column names, locical()}
	\item{display.rownames}{display row names, locical()}
	\item{hlines}{display horizontal lines, locical()}
	\item{vlines}{display vertical lines, locical()}
	\item{mar}{margin of table plot}
	\item{title}{main title of graphical table}
	\item{col1}{first color used to highlight cells}
	\item{col2}{second color used to highlight cells}
	\item{column1}{column1}
	\item{column2}{column2}
	\item{mwidth}{adjustment between cells}
	\item{cellwidth}{width of cells}
	\item{cellheight}{height of cells}
	
	
}
\details{interactive, clickable tables}
\author{Alexander Kowarik, Bernhard Meindl}
\keyword{methods}