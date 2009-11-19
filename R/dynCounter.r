dynCounterCall <- function() {
	pressButton <- function() {
		setwd(pathGUI)		
	    tkdestroy(ttDyn)	
		rm(list=ls(env=TGUIenv), envir=TGUIenv)
	}		
	genAuswertungsFunktion <- function(ex) {
		comm <- paste(ex, "(eval=TRUE)", sep="")
		eval(parse(text=comm))
	}		
	# returns available exercises along with corresponding evalutation-functions
	returnExamplesWithAusw <- function() {
		contents <- aTget("contents")	
		auswertungC <- contents$Auswertung
		examplesC <- as.character(contents$Funktion)
		examplesC <- as.vector(sapply(examplesC, function(x) { substr(x, 1, nchar(x)-2) } ) )
		
		examplesAE <- read.table("adminExercise.txt", sep=",", header=F)
		examplesAE <- as.character(examplesAE[,1])			
		examples <- examplesC[which(examplesC %in% examplesAE & auswertungC == 1)]

		return(examples)
	}
	# plot-function for dynamic counter
	PlotDynamicCounter <- function(maxUsers=16, plotCol="cornflowerblue") {
		calcValues <- function(examples) {
			counter <- list()
			for (i in examples) {
				file <- paste(i, ".txt", sep="")
				exists <- file.exists(file)
				if(exists==FALSE) {
					counter[[i]] <- 0
				}
				else {
					tmp <- read.table(file, sep=",", header=FALSE)
					if(ncol(tmp)==1) {
						counter[[i]] <- nrow(tmp)
					}
					else {
						if(is.integer(tmp[1,1])) {
							counter[[i]] <- nrow(tmp) / max(tmp[,1])	
						}
						else {
							counter[[i]] <- nrow(tmp)
						}						
					}				
				}
			}
			return(counter)
		}
		
		par(bg="white", las=3)
		par(mar=c(7, 3, 3, 2) + 0.1) 		

		examples <- returnExamplesWithAusw()
		counter <- unlist(calcValues(examples))	
		barplot(counter, space=3, horiz=FALSE, col=plotCol, ylim=c(0, maxUsers))	
	}	
	
	OpenWindow(window=ttDyn, title="Dynamic counter")	

	filename <- "adminExercise.txt"
	header <- "Dynamic counter"
	
	totFrame <- tkframe(MainFrame)
	tkgrid(totFrame)
	
	tkgrid(tklabel(totFrame, text=""));					
	
	HFrame <- tkframe(totFrame)
	tkgrid(HFrame)
	q1 <- tklabel(HFrame, text=header, font=setFont(size="extralarge", bold=TRUE))
	tkgrid(q1)	
	tkgrid.configure(q1, columnspan=1)
	
	plotFrame <- tkframe(totFrame)	
	tkgrid(plotFrame)
	img <- tkrplot(plotFrame, fun=PlotDynamicCounter, hscale=2, vscale=1.6)
	tkgrid(img)		
	
	tkgrid(tklabel(totFrame, text=""));		
	
	buttonFrame <- tkframe(totFrame)
	tkgrid(buttonFrame)		
	button <- tkbutton(buttonFrame, text="Close (and kill) dynamic updating graph", command=pressButton, fg="darkgreen")
	tkgrid(button)	

	tkgrid(tklabel(totFrame, text=""));	
	
	auswFrame <- tkframe(totFrame)
	tkgrid(auswFrame)
	q2 <- tklabel(auswFrame, text="Jump to evaluation for the open examples", font=setFont(size="extralarge", bold=TRUE))
	tkgrid(q2)	
	tkgrid.configure(q2, columnspan=5)	
	
	tkgrid(tklabel(totFrame, text=""));	
	
	examples <- returnExamplesWithAusw()
	
	answerFrame <- tkframe(totFrame)
	tkgrid(answerFrame)
	but <- list()
	for (i in 1:length(examples)) {
		but[[i]] <- tkbutton(answerFrame, text=examples[i], command=eval(parse(text=paste("function() { aTassign(\"ind\", FALSE); genAuswertungsFunktion(\"", examples[i], "\") }", sep=""))))
	}
	
	cols <- 8
	nrEx <- length(examples)
	for (i in 0:(ceiling(nrEx/cols)-1)) {
		str <- "tkgrid("
		
		remaining <- (nrEx - i*cols)
		if(remaining >= cols) {			
			for (j in 1:cols) {
				str <- paste(str, "but[[",i*cols+j,"]],", sep="")
			}
			str <- substr(str, 1, nchar(str)-1)
			str <- paste(str, ")", sep="")
		}
		else {
			for (j in 1:remaining) {
				str <- paste(str, "but[[",i*cols+j,"]],", sep="")
			}
			str <- substr(str, 1, nchar(str)-1)
			str <- paste(str, ")", sep="")
		}
		eval(parse(text=str))
	}	

	linkFrame <- tkframe(totFrame)
	tkgrid(linkFrame)	
	
	aTassign("ind", ifelse(tclvalue(tkwinfo("exists", ttDyn))==1, TRUE, FALSE))

	while(aTget("ind")==TRUE) {
		Sys.sleep(.05)
		aTassign("ind", ifelse(tclvalue(tkwinfo("exists", ttDyn))==1, TRUE, FALSE))
		
		if(as.integer(aTget("ind"))==1) {
			tkrreplot(img)
		}
		Sys.sleep(.3)	
	}
}